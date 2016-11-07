/**
 * Copyright (c) 2013, The National Archives <digitalpreservation@nationalarchives.gov.uk>
 * http://www.nationalarchives.gov.uk
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package uk.gov.nationalarchives.csv.validator.schema.v1_0

import java.io.File
import java.net.{URI, URISyntaxException}

import org.joda.time.format.{DateTimeFormat, DateTimeFormatterBuilder, ISODateTimeFormat}
import org.joda.time.{DateTime, Interval}
import uk.gov.nationalarchives.csv.validator.Util.{FileSystem, TypedPath}
import uk.gov.nationalarchives.csv.validator.api.CsvValidator._
import uk.gov.nationalarchives.csv.validator.metadata.Row
import uk.gov.nationalarchives.csv.validator.schema._
import java.util.regex.{Pattern, Matcher}
import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Try
import scalax.file.{Path, PathSet}
import scalaz.Scalaz._
import scalaz.{Failure => FailureZ, Success => SuccessZ, _}

case class OrRule(left: Rule, right: Rule) extends Rule("or") {
  override def evaluate(columnIndex: Int, row: Row,  schema: Schema, mayBeLast: Option[Boolean] = None): RuleValidation[Any] = {
    left.evaluate(columnIndex, row, schema, mayBeLast) match {
      case s @ SuccessZ(_) => s

      case FailureZ(_) => right.evaluate(columnIndex, row, schema,  mayBeLast) match {
        case s @ SuccessZ(_) => s
        case FailureZ(_) => fail(columnIndex, row, schema)
      }
    }
  }

  override def toError = s"""${left.toError} $ruleName ${right.toError}"""
}

case class ParenthesesRule(rules: List[Rule]) extends Rule("parentheses") {

  override def evaluate(columnIndex: Int, row: Row, schema: Schema, mayBeLast: Option[Boolean] = None): RuleValidation[Any] = {
    val v = for (rule <- rules) yield {
      rule.evaluate(columnIndex, row, schema, mayBeLast)
    }

    v.sequence[RuleValidation, Any]
  }


  override def toError = {
    val paramErrs = rules.map(_.toError).mkString(" ")
    s"""($paramErrs)""" + (if (argProviders.isEmpty) "" else "(" + argProviders.foldLeft("")((a, b) => (if (a.isEmpty) "" else a + ", ") + b.toError) + ")")
  }
}

case class IfRule(condition: Rule, rules: List[Rule], elseRules: Option[List[Rule]]) extends Rule("if") {

  override def evaluate(columnIndex: Int, row: Row, schema: Schema, mayBeLast: Option[Boolean] = None): RuleValidation[Any] = {

    def conditionValid: Boolean = {
      val (cellValue,idx) = findColumnRefence(condition) match {
        case Some(columnRef) =>
          (columnRef.referenceValueEx(columnIndex, row, schema), columnIdentifierToIndex(schema, columnRef.ref))
        case None =>
          (row.cells(columnIndex).value, columnIndex)
      }
      condition.valid(cellValue, schema.columnDefinitions(columnIndex), idx, row, schema)
    }

    val v = if (conditionValid) {
      for (rule <- rules) yield {
        rule.evaluate(columnIndex, row, schema)
      }
    } else {
      elseRules.map{ er =>
        for (rule <- er) yield {
          rule.evaluate(columnIndex, row, schema)
        }
      }.getOrElse(Nil)
    }

    v.sequence[RuleValidation, Any]
  }

  override def toError = {
    val paramErrs = rules.map( _.toError).mkString(" ")
    s"""($paramErrs)""" + (if (argProviders.isEmpty) "" else "(" + argProviders.foldLeft("")((a, b) => (if (a.isEmpty) "" else a + ", ") + b.toError) + ")")
  }
}

case class RegExpRule(regex: String) extends Rule("regex") {
  override def valid(cellValue: String, columnDefinition: ColumnDefinition, columnIndex: Int, row: Row, schema: Schema, mayBeLast: Option[Boolean] = None): Boolean = {
    val regexp = if (columnDefinition.directives.contains(IgnoreCase())) "(?i)" + regex else regex
    RegexCache.getCompiledRegex(regexp).matcher(cellValue).matches()
  }

  override def toError = {
    s"""$ruleName("$regex")"""
  }
}

//TODO note the use of `Seq(rootPath): _*` when extending Rule, this is to workaround this bug https://issues.scala-lang.org/browse/SI-7436. This pattern is repeated below!
case class FileExistsRule(pathSubstitutions: List[(String,String)], enforceCaseSensitivePathChecks: Boolean, rootPath: ArgProvider = Literal(None)) extends Rule("fileExists", Seq(rootPath): _*) {

  override def valid(filePath: String, columnDefinition: ColumnDefinition, columnIndex: Int, row: Row, schema: Schema, mayBeLast: Option[Boolean] = None) = {

    val ruleValue = rootPath.referenceValue(columnIndex, row, schema)

    val fileExists = ruleValue match {
      case Some(rp) =>  new FileSystem(rp, filePath, pathSubstitutions).exists(enforceCaseSensitivePathChecks)
      case None =>   new FileSystem(filePath, pathSubstitutions).exists(enforceCaseSensitivePathChecks)
    }
    fileExists
  }

  override def toError = s"""$ruleName""" + (if (rootPath == Literal(None)) "" else s"""(${rootPath.toError})""")
}

case class InRule(inValue: ArgProvider) extends Rule("in", Seq(inValue): _*) {
  override def valid(cellValue: String, columnDefinition: ColumnDefinition, columnIndex: Int, row: Row, schema: Schema, mayBeLast: Option[Boolean] = None): Boolean = {
    val ruleValue = inValue.referenceValue(columnIndex, row, schema)

    val (rv, cv) = if (columnDefinition.directives.contains(IgnoreCase())) (ruleValue.get.toLowerCase, cellValue.toLowerCase) else (ruleValue.get, cellValue)
    rv contains cv
  }
}

case class IsRule(isValue: ArgProvider) extends Rule("is", Seq(isValue): _*) {
  override def valid(cellValue: String, columnDefinition: ColumnDefinition, columnIndex: Int, row: Row, schema: Schema, mayBeLast: Option[Boolean]  = None): Boolean = {
    val ruleValue = isValue.referenceValue(columnIndex, row, schema)

    val (rv, cv) = if (columnDefinition.directives.contains(IgnoreCase())) (ruleValue.get.toLowerCase, cellValue.toLowerCase) else (ruleValue.get, cellValue)
    cv == rv
  }
}


case class NotRule(notValue: ArgProvider) extends Rule("not", Seq(notValue): _*) {
  override def valid(cellValue: String, columnDefinition: ColumnDefinition, columnIndex: Int, row: Row, schema: Schema,mayBeLast: Option[Boolean] = None): Boolean = {
    val ruleValue = notValue.referenceValue(columnIndex, row, schema)

    val (rv, cv) = if (columnDefinition.directives.contains(IgnoreCase())) (ruleValue.get.toLowerCase, cellValue.toLowerCase) else (ruleValue.get, cellValue)
    cv != rv
  }
}

case class StartsRule(startsValue: ArgProvider) extends Rule("starts", Seq(startsValue): _*) {
  override def valid(cellValue: String, columnDefinition: ColumnDefinition, columnIndex: Int, row: Row, schema: Schema, mayBeLast: Option[Boolean] = None): Boolean = {
    val ruleValue = startsValue.referenceValue(columnIndex, row, schema)

    val (rv, cv) = if (columnDefinition.directives.contains(IgnoreCase())) (ruleValue.get.toLowerCase, cellValue.toLowerCase) else (ruleValue.get, cellValue)
    cv startsWith rv
  }
}

case class EndsRule(endsValue: ArgProvider) extends Rule("ends", Seq(endsValue): _*) {
  override def valid(cellValue: String, columnDefinition: ColumnDefinition, columnIndex: Int, row: Row, schema: Schema, mayBeLast: Option[Boolean] = None): Boolean = {
    val ruleValue = endsValue.referenceValue(columnIndex, row, schema)

    val (rv, cv) = if (columnDefinition.directives.contains(IgnoreCase())) (ruleValue.get.toLowerCase, cellValue.toLowerCase) else (ruleValue.get, cellValue)
    cv endsWith rv
  }
}

case class EmptyRule() extends Rule("empty") {
  override def valid(cellValue: String, columnDefinition: ColumnDefinition, columnIndex: Int, row: Row, schema: Schema, mayBeLast: Option[Boolean] = None): Boolean = {
    cellValue.isEmpty
  }
}

case class NotEmptyRule() extends Rule("notEmpty") {
  override def valid(cellValue: String, columnDefinition: ColumnDefinition, columnIndex: Int, row: Row, schema: Schema, mayBeLast: Option[Boolean] = None): Boolean = {
    !cellValue.isEmpty
  }
}

case class UriRule() extends Rule("uri") {
  override def valid(cellValue: String, columnDefinition: ColumnDefinition, columnIndex: Int, row: Row, schema: Schema, mayBeLast: Option[Boolean] = None): Boolean = {
    try {
      val uri = new URI(cellValue)
      true
    } catch {
      case use: URISyntaxException => false
    }
  }
}



object IsoDateTimeParser extends DateParser {
  val isoDateTimeFormatter = ISODateTimeFormat.dateTimeParser().withOffsetParsed()
  def parse(dateStr: String): Try[DateTime] = Try(isoDateTimeFormatter.parseDateTime(dateStr))
}

object XsdDateParser extends DateParser {
  val xsdDateFormatter = new DateTimeFormatterBuilder()
    .append(ISODateTimeFormat.dateElementParser)
    .appendOptional(
      new DateTimeFormatterBuilder()
        .appendTimeZoneOffset("Z", true, 2, 4).toParser
    ).toFormatter

  def parse(dateStr: String): Try[DateTime] = Try(xsdDateFormatter.parseDateTime(dateStr))
}

object IsoTimeParser extends DateParser {
  val isoTimeFormatter = ISODateTimeFormat.timeParser
  def parse(dateStr: String): Try[DateTime] = Try(isoTimeFormatter.parseDateTime(dateStr))
}

object UkDateParser extends DateParser {
  val fmt = DateTimeFormat.forPattern(UkDateFormat)
  def parse(dateStr: String): Try[DateTime] = Try(fmt.parseDateTime(dateStr))
}

abstract class DateRangeRule(name: String, dateRegex: String, dateParser: DateParser) extends Rule(name) {
  import dateParser.parse
  val from: String
  val to: String
  lazy val fromDate = parse(from)
  lazy val toDate = parse(to)

  override def valid(cellValue: String, columnDefinition: ColumnDefinition, columnIndex: Int, row: Row, schema: Schema, mayBeLast: Option[Boolean] = None): Boolean = {
    cellValue matches dateRegex match {
      case true => {
        val inRange = for ( frmDt <- fromDate; toDt <- toDate; cellDt <- parse(cellValue)) yield {
          val interval = new Interval(frmDt,toDt.plusMillis(1))
          interval.contains(cellDt)
        }

        inRange.getOrElse(false)
      }

      case _ => false
    }
  }

  override def toError = s"""$ruleName("$from, $to")"""
}



case class XsdDateTimeRule() extends DateRule("xDateTime", XsdDateTimeRegex, IsoDateTimeParser)

case class XsdDateTimeRangeRule(from: String, to: String) extends DateRangeRule("xDateTime", XsdDateTimeRegex, IsoDateTimeParser)

case class XsdDateRule() extends DateRule("xDate", XsdDateRegex, XsdDateParser)

case class XsdDateRangeRule(from: String, to: String) extends DateRangeRule("xDate",  XsdDateRegex, XsdDateParser)

case class UkDateRule() extends DateRule("ukDate", UkDateRegex, UkDateParser)

case class UkDateRangeRule(from: String, to: String) extends DateRangeRule("ukDate", UkDateRegex, UkDateParser)

case class XsdTimeRule() extends DateRule("xTime", XsdTimeOptionalTimeZoneRegex, IsoTimeParser)

case class XsdTimeRangeRule(from: String, to: String) extends DateRangeRule("xTime", XsdTimeOptionalTimeZoneRegex, IsoTimeParser)

case class PartUkDateRule() extends PatternRule("partUkDate", PartUkDateRegex)

case class Uuid4Rule() extends PatternRule("uuid4", Uuid4Regex)

case class PositiveIntegerRule() extends PatternRule("positiveInteger", PositiveIntegerRegex)

case class UniqueRule() extends Rule("unique") {
  val distinctValues = mutable.HashMap[String, Int]()

  override def evaluate(columnIndex: Int, row: Row, schema: Schema, mayBeLast: Option[Boolean] = None): RuleValidation[Any] = {
    val columnDefinition = schema.columnDefinitions(columnIndex)

    def originalValue: Option[String] = {
      val cellValue = cellValueCorrectCase
      if (distinctValues contains cellValue) Some(cellValue) else None
    }

    def cellValueCorrectCase = if (columnDefinition.directives contains IgnoreCase()) cellValue(columnIndex,row,schema).toLowerCase else cellValue(columnIndex,row,schema)

    originalValue match {
      case None => distinctValues.put(cellValueCorrectCase, row.lineNumber); true.successNel
      case Some(o) => {
        s"$toError fails for line: ${row.lineNumber}, column: ${columnDefinition.id}, ${toValueError(row,columnIndex)} (original at line: ${distinctValues(o)})".failureNel[Any]
      }
    }
  }
}

case class UniqueMultiRule(columns: List[ColumnReference]) extends Rule("unique(") {
  val SEPARATOR:Char = 0x07 // BEL
  val distinctValues = mutable.HashMap[String, Int]()

  override def evaluate(columnIndex: Int, row: Row, schema: Schema, mayBeLast: Option[Boolean] = None): RuleValidation[Any] = {
    val columnDefinition = schema.columnDefinitions(columnIndex)

    def secondaryValues = columns.map(_.referenceValue(columnIndex, row, schema)).mkString(SEPARATOR.toString)

    def uniqueString: String =  cellValue(columnIndex,row,schema) + SEPARATOR +  secondaryValues

    def originalValue: Option[String] = {
      val cellValue = cellValueCorrectCase
      if (distinctValues contains cellValue) Some(cellValue) else None
    }

    def cellValueCorrectCase = if (columnDefinition.directives contains IgnoreCase) uniqueString.toLowerCase else uniqueString

    originalValue match {
      case None => distinctValues.put(cellValueCorrectCase, row.lineNumber); true.successNel
      case Some(o) => {
        s"$toError ${columns.map(_.toError).mkString(", ")} ) fails for line: ${row.lineNumber}, column: ${columnDefinition.id}, ${toValueError(row,columnIndex)} (original at line: ${distinctValues(o)})".failureNel[Any]
      }
    }
  }
}

case class ChecksumRule(rootPath: ArgProvider, file: ArgProvider, algorithm: String, pathSubstitutions: List[(String,String)], enforceCaseSensitivePathChecks: Boolean = false) extends Rule("checksum", Seq(rootPath, file): _*) with FileWildcardSearch[String] {

  def this(file: ArgProvider, algorithm: String, pathSubstitutions: List[(String,String)], enforceCaseSensitivePathChecks: Boolean) = this(Literal(None), file, algorithm, pathSubstitutions, enforceCaseSensitivePathChecks)
  def this(file: ArgProvider, algorithm: String, enforceCaseSensitivePathChecks: Boolean) = this(Literal(None), file, algorithm, List.empty[(String,String)], enforceCaseSensitivePathChecks)

  override def evaluate(columnIndex: Int, row: Row, schema: Schema, mayBeLast: Option[Boolean] = None): RuleValidation[Any] = {
    val columnDefinition = schema.columnDefinitions(columnIndex)

    search(filename(columnIndex, row, schema)) match {
      case SuccessZ(hexValue: String) if hexValue == cellValue(columnIndex,row,schema) => true.successNel[String]
      case SuccessZ(hexValue: String) => s"""$toError file "${TypedPath(filename(columnIndex, row, schema)._1 + filename(columnIndex, row, schema)._2).toPlatform}" checksum match fails for line: ${row.lineNumber}, column: ${columnDefinition.id}, ${toValueError(row,columnIndex)}. Computed checksum value:"${hexValue}"""".failureNel[Any]
      case FailureZ(errMsg) => s"$toError ${errMsg.head} for line: ${row.lineNumber}, column: ${columnDefinition.id}, ${toValueError(row,columnIndex)}".failureNel[Any]
    }
  }

  override def toError = {
    if (rootPath.toError.isEmpty) s"""$ruleName(file(${file.toError}), "$algorithm")"""
    else s"""$ruleName(file(${rootPath.toError}, ${file.toError}), "$algorithm")"""
  }

  private def filename(columnIndex: Int, row: Row, schema: Schema): (String,String) = {
    val f = file.referenceValue(columnIndex, row, schema).get

    rootPath.referenceValue(columnIndex, row, schema) match {
      case None =>
        ("", f)
      case Some(r: String) if(r.charAt(r.length - 1) == TypedPath(r).separator) =>
        (r, f)
      case Some(r) =>
        (r + TypedPath(r).separator, f)
    }
  }

  def matchWildcardPaths(matchList: PathSet[Path],fullPath: String): ValidationNel[String, String] = matchList.size match {
    case 1 => calcChecksum(matchList.head.path)
    case 0 => s"""no files for $fullPath found""".failureNel[String]
    case _ => s"""multiple files for ${TypedPath(fullPath).toPlatform} found""".failureNel[String]
  }

  def matchSimplePath(fullPath: String): ValidationNel[String, String]  = calcChecksum(fullPath)

  def calcChecksum(file: String): ValidationNel[String, String] = {

    def checksum(f: File): ValidationNel[String, String] = {

      import scalaz.stream._

      def getHash(algorithm: String) : Process1[scodec.bits.ByteVector, scodec.bits.ByteVector] = {
        val hashes = Map(
          ("md2", () => hash.md2),
          ("md5", () => hash.md5),
          ("sha1", () => hash.sha1),
          ("sha256", () => hash.sha256),
          ("sha384", () => hash.sha384),
          ("sha512", () => hash.sha512)
        )
        hashes(algorithm.toLowerCase().replace("-", ""))()
      }

      val bufSize = 16384 //16KB
      Process.constant(bufSize)
        .toSource
        .through(io.fileChunkR(f.getAbsolutePath, bufSize))
        .pipe(getHash(algorithm))
        .map(_.toHex)
        .runLast
        .unsafePerformSyncAttempt
        .validation
        .leftMap(_.getMessage)
        .rightMap(_.getOrElse("NO CHECKSUM"))
        .toValidationNel
    }

    FileSystem.createFile(file) match {
      case scala.util.Success(f) if(f.exists) =>
        if(enforceCaseSensitivePathChecks) {
          if(FileSystem.caseSensitivePathMatchesFs(f)) {
            checksum(f)
          } else {
            s"""file "${FileSystem.file2PatformDependent(file)}" not found""".failureNel[String]
          }
        } else {
          checksum(f)
        }

      case scala.util.Failure(_) =>
        s"""file "${FileSystem.file2PatformDependent(file)}" not found""".failureNel[String]
    }
  }
}

case class FileCountRule(rootPath: ArgProvider, file: ArgProvider, pathSubstitutions: List[SubstitutePath] = List.empty) extends Rule("fileCount", Seq(rootPath, file): _*) with FileWildcardSearch[Int] {
  def this(file: ArgProvider, pathSubstitutions: List[SubstitutePath]) = this(Literal(None), file, pathSubstitutions)
  def this(rootPath: Literal, file: Literal) = this(rootPath, file,  List.empty)

  override def evaluate(columnIndex: Int, row: Row, schema: Schema, mayBeLast: Option[Boolean] = None): RuleValidation[Any] = {
    val columnDefinition = schema.columnDefinitions(columnIndex)

    Try(cellValue(columnIndex,row,schema).toInt) match {
      case scala.util.Success(cellCount) =>
        search(filename(columnIndex, row, schema)) match {
          case SuccessZ(count: Int) if count == cellCount => true.successNel[String]
          case SuccessZ(count: Int) => s"$toError found $count file(s) for line: ${row.lineNumber}, column: ${columnDefinition.id}, ${toValueError(row,columnIndex)}".failureNel[Any]
          case FailureZ(errMsg) => s"$toError ${errMsg.head} for line: ${row.lineNumber}, column: ${columnDefinition.id}, ${toValueError(row,columnIndex)}".failureNel[Any]
        }

      case scala.util.Failure(_) =>  s"$toError '${cellValue(columnIndex,row,schema)}' is not a number for line: ${row.lineNumber}, column: ${columnDefinition.id}, ${toValueError(row,columnIndex)}".failureNel[Any]
    }
  }

  override def toError = {
    if (rootPath.toError.isEmpty) s"""$ruleName(file(${file.toError}))"""
    else s"""$ruleName(file(${rootPath.toError}, ${file.toError}))"""
  }

  private def filename(columnIndex: Int, row: Row, schema: Schema): (FilePathBase, FileName) = {
    val f = file.referenceValue(columnIndex, row, schema).get

    rootPath.referenceValue(columnIndex, row, schema) match {
      case None => ("", f)
      case Some(r: String) if r.endsWith("/") => (r, f)
      case Some(r) => (r + "/", f)
    }
  }

  def matchWildcardPaths(matchList: PathSet[Path],fullPath: String): ValidationNel[String, Int] = matchList.size.successNel[String]

  def matchSimplePath(fullPath: String): ValidationNel[String, Int]  = 1.successNel[String]  // file found so ok
}

trait FileWildcardSearch[T] {

  val pathSubstitutions: List[SubstitutePath]
  def matchWildcardPaths(matchList: PathSet[Path], fullPath: String): ValidationNel[String, T]
  def matchSimplePath(fullPath: String): ValidationNel[String, T]

  val wildcardPath = (p: Path, matchPath: String) => p.descendants( p.matcher( matchPath))
  val wildcardFile = (p: Path, matchPath: String) => p.children( p.matcher( "**/" +matchPath))

  //TODO consider re-writing the FileSystem stuff to use TypedPath now that we have toPlatform - just need a better File approach and handling of parent/child - maybe use scalax.file.Path

  def findBase(path:String): (TypedPath, String) = {

    @tailrec
    def findBaseRecur(p : TypedPath, subPath: String) : (TypedPath, String) = {
      p.parent match {
        case None =>
          (p.thisFolder, subPath)

        case Some(parent) =>
          if(!parent.path.contains("*"))
            (parent, subPath)
          else
            findBaseRecur(parent, parent.name + p.separator + subPath)
      }
    }

    val typedBasePath = TypedPath(path)
    findBaseRecur(typedBasePath, typedBasePath.name)

  }

  def search(filePaths: (FilePathBase, FileName)): ValidationNel[String, T] = {
    try {
      val fullPath = new FileSystem(None, filePaths._1 + filePaths._2, pathSubstitutions).expandBasePath
      val (basePath, matchPath) = findBase(fullPath)

      val path: Option[Path] = {
        FileSystem.createFile( basePath.toPlatform.toString ) match {
          case scala.util.Success(f) => Some(Path(f))
          case scala.util.Failure(_) => None
        }
      }

      def pathString = s"${filePaths._1} (localfile: ${TypedPath(fullPath).toPlatform})"

      def findMatches(wc: (Path, String) => PathSet[Path] ): ValidationNel[String, T] = {
        path match {
          case Some(p) =>  matchWildcardPaths( wc(p, matchPath ), fullPath )
          case None => "no file".failureNel[T]
        }
      }

      def basePathExists: Boolean =   filePaths._1.length>0 && (!(FileSystem.createFile( basePath.toPlatform.toString ) match {
        case scala.util.Success(f) =>   f.exists
        case scala.util.Failure(_) => false
      }))

      def wildcardNotInRoot: Boolean = filePaths._1.contains("*")

      def matchUsesWildDirectory: Boolean = matchPath.contains("**")

      def matchUsesWildFiles: Boolean = matchPath.contains("*")

      def fileExists: Boolean = {
        val platformPath = basePath.toPlatform
        val path = platformPath.toString + platformPath.separator + matchPath

        FileSystem.createFile( path ) match {
          case scala.util.Success(file) =>   file.exists
          case scala.util.Failure(_) => false
        }
      }

      if(basePathExists) {
        s"""incorrect basepath $pathString found""".failureNel[T]
      } else if(wildcardNotInRoot ) {
        s"""root $pathString should not contain wildcards""".failureNel[T]
      } else if(matchUsesWildDirectory) {
        findMatches(wildcardPath)
      } else if(matchUsesWildFiles) {
        findMatches(wildcardFile)
      } else if(!fileExists) {
        s"""file "${TypedPath(fullPath).toPlatform}" not found""".failureNel[T]
      } else {
        matchSimplePath(basePath + System.getProperty("file.separator") + matchPath)
      }
    } catch {
      case err:Throwable =>
        err.getMessage.failureNel[T]
    }
  }
}

case class RangeRule(min: BigDecimal, max: BigDecimal) extends Rule("range") {

  override def valid(cellValue: String, columnDefinition: ColumnDefinition, columnIndex: Int, row: Row, schema: Schema,  mayBeLast: Option[Boolean] = None): Boolean = {
    Try[BigDecimal]( BigDecimal(cellValue)) match {
      case scala.util.Success(callDecimal) => (callDecimal >= min && callDecimal <= max  )
      case _ => false
    }
  }

  override def toError = s"""$ruleName($min,$max)"""
}

case class LengthRule(from: Option[String], to: String) extends Rule("length") {

  def toValue: Int = if (to == "*") Int.MaxValue else to.toInt
  def fromValue: Int =  if (from.get == "*") 0 else from.get.toInt

  override def valid(cellValue: String, columnDefinition: ColumnDefinition, columnIndex: Int, row: Row, schema: Schema, mayBeLast: Option[Boolean] = None): Boolean = {
    val cellLen = cellValue.length

    from match {
      case None => if ( to=="*") true else cellLen == to.toInt
      case Some(_) => cellLen >= fromValue && cellLen <= toValue
    }
  }

  override def toError = if(from.isDefined) s"""$ruleName(${from.get},$to)""" else s"""$ruleName($to)"""
}

case class AndRule(left: Rule, right: Rule) extends Rule("and") {
  override def evaluate(columnIndex: Int, row: Row, schema: Schema, mayBeLast: Option[Boolean] = None): RuleValidation[Any] = {
    left.evaluate(columnIndex, row, schema) match {
      case s @ FailureZ(_) => fail(columnIndex, row, schema)

      case SuccessZ(_) => right.evaluate(columnIndex, row, schema) match {
        case s @ SuccessZ(_) => s
        case FailureZ(_) => fail(columnIndex, row, schema)
      }
    }
  }

  override def toError = s"""${left.toError} $ruleName ${right.toError}"""
}
