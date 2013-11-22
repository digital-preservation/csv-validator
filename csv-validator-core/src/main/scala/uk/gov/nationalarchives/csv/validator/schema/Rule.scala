/**
 * Copyright (c) 2013, The National Archives <digitalpreservation@nationalarchives.gov.uk>
 * http://www.nationalarchives.gov.uk
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package uk.gov.nationalarchives.csv.validator.schema

import scalax.file.{PathSet, Path}
import scalaz._, Scalaz._
import java.io.{BufferedInputStream, FileInputStream, File}
import scala.util.parsing.input.Positional
import scala.collection.mutable
import java.security.MessageDigest
import uk.gov.nationalarchives.csv.validator.metadata.Row
import util.Try
import annotation.tailrec
import java.net.{URISyntaxException, URI}
import org.joda.time.{Interval, LocalTime, DateTime}
import org.joda.time.format.{DateTimeFormatter, DateTimeFormatterBuilder, ISODateTimeFormat, DateTimeFormat}
import scalaz.{Success => SuccessZ, Failure => FailureZ}
import java.util.regex.{Pattern, Matcher}
import scala.Some
import uk.gov.nationalarchives.csv.validator.{UNIX_FILE_SEPARATOR, WINDOWS_FILE_SEPARATOR, FILE_SEPARATOR, URI_PATH_SEPARATOR}
import uk.gov.nationalarchives.csv.validator.api.CsvValidator.SubstitutePath

abstract class Rule(name: String, val argProviders: ArgProvider*) extends Positional {

  type RuleValidation[A] = ValidationNel[String, A]

  var explicitColumn: Option[String] = None

  def evaluate(columnIndex: Int, row: Row, schema: Schema): RuleValidation[Any] = {
    if (valid(cellValue(columnIndex, row, schema), schema.columnDefinitions(columnIndex), columnIndex, row, schema)) true.successNel[String] else fail(columnIndex, row, schema)
  }

  def valid(cellValue: String, columnDefinition: ColumnDefinition, columnIndex: Int, row: Row, schema: Schema): Boolean

  def fail(columnIndex: Int, row: Row, schema: Schema): RuleValidation[Any] = {
    val columnDefinition = schema.columnDefinitions(columnIndex)
    s"$toError fails for line: ${row.lineNumber}, column: ${columnDefinition.id}, ${toValueError(row,columnIndex)}".failNel[Any]
  }

  def cellValue(columnIndex: Int, row: Row, schema: Schema) = explicitColumn match {
    case Some(columnName) => row.cells(columnNameToIndex(schema, columnName)).value
    case None => row.cells(columnIndex).value
  }

  def explicitName = explicitColumn match {
    case Some(colName) => "$" + colName + "/"
    case None => ""
  }

  def ruleName = explicitName + name

  def columnNameToIndex(schema: Schema, name: String): Int = {
    try {
      schema.columnDefinitions.zipWithIndex.filter{ case (c,i) => c.id == name}.head._2
    } catch {
      // this should be fixed in the uk.gov.nationalarchives.csv.validator.schema validator, preventing this from ever happening
      case _: java.util.NoSuchElementException => println( s"Error:   Unable to find: $name for line: ${pos.line}, column: ${pos.column}"); 0
      case _: Throwable => println( s"Error: with: $name"); 0
    }
  }

  def toValueError(row: Row, columnIndex:Int ) =  s"""value: ${'"'}${row.cells(columnIndex).value}${'"'}"""

  def toError = s"""$ruleName""" + (if (argProviders.isEmpty) "" else "(" + argProviders.foldLeft("")((a, b) => (if (a.isEmpty) "" else a + ", ") + b.toError) + ")")
}

case class OrRule(left: Rule, right: Rule) extends Rule("or") {
  override def evaluate(columnIndex: Int, row: Row, schema: Schema): RuleValidation[Any] = {
    left.evaluate(columnIndex, row, schema) match {
      case s @ SuccessZ(_) => s

      case FailureZ(_) => right.evaluate(columnIndex, row, schema) match {
        case s @ SuccessZ(_) => s
        case FailureZ(_) => fail(columnIndex, row, schema)
      }
    }
  }

  def valid(cellValue: String, columnDefinition: ColumnDefinition, columnIndex: Int, row: Row, schema: Schema): Boolean = {
    evaluate(columnIndex, row, schema) match {
      case FailureZ(_) => false
      case SuccessZ(_) => true
    }
  }

  override def toError = s"""${left.toError} $ruleName ${right.toError}"""
}

case class ParenthesesRule(rules: List[Rule]) extends Rule("parentheses") {

  override def evaluate(columnIndex: Int, row: Row, schema: Schema): RuleValidation[Any] = {
    val v = for (rule <- rules) yield {
      rule.evaluate(columnIndex, row, schema)
    }

    v.sequence[RuleValidation, Any]
  }

  def valid(cellValue: String, columnDefinition: ColumnDefinition, columnIndex: Int, row: Row, schema: Schema): Boolean = {
    evaluate(columnIndex, row, schema) match {
      case FailureZ(_) => false
      case SuccessZ(_) => true
    }
  }

  override def toError = {
    val paramErrs = rules.map(_.toError).mkString(" ")
    s"""($paramErrs)""" + (if (argProviders.isEmpty) "" else "(" + argProviders.foldLeft("")((a, b) => (if (a.isEmpty) "" else a + ", ") + b.toError) + ")")
  }
}

case class IfRule(condition: Rule, rules: List[Rule], elseRules: Option[List[Rule]]) extends Rule("if") {

  override def evaluate(columnIndex: Int, row: Row, schema: Schema): RuleValidation[Any] = {
    val (cellValue,idx) = condition.explicitColumn match {
      case Some(columnName) => (row.cells(columnNameToIndex(schema, columnName)).value, columnNameToIndex(schema, columnName) )
      case None => (row.cells(columnIndex).value, columnIndex)
    }

    val v = if (condition.valid(cellValue, schema.columnDefinitions(columnIndex), idx, row, schema)) {
     for (rule <- rules) yield {
        rule.evaluate(columnIndex, row, schema)
      }
    } else {
      if (elseRules.isDefined) {
        for (rule <- elseRules.get) yield {
          rule.evaluate(columnIndex, row, schema)
        }
      } else {
        Nil
      }
    }

    v.sequence[RuleValidation, Any]
  }

  def valid(cellValue: String, columnDefinition: ColumnDefinition, columnIndex: Int, row: Row, schema: Schema): Boolean = {
    evaluate(columnIndex, row, schema) match {
      case FailureZ(_) => false
      case SuccessZ(_) => true
    }
  }

  override def toError = {
    val paramErrs = rules.map( _.toError).mkString(" ")
    s"""($paramErrs)""" + (if (argProviders.isEmpty) "" else "(" + argProviders.foldLeft("")((a, b) => (if (a.isEmpty) "" else a + ", ") + b.toError) + ")")
  }
}

case class RegexRule(regex: String) extends Rule("regex") {
  def valid(cellValue: String, columnDefinition: ColumnDefinition, columnIndex: Int, row: Row, schema: Schema): Boolean = {

    val regexp = if (columnDefinition.directives.contains(IgnoreCase())) "(?i)" + regex else regex
    cellValue matches regexp
  }

  override def toError = {
    s"""$ruleName("$regex")"""
  }
}

case class FileExistsRule(pathSubstitutions: List[(String,String)], rootPath: ArgProvider = Literal(None) ) extends Rule("fileExists", rootPath) {
  def valid(filePath: String, columnDefinition: ColumnDefinition, columnIndex: Int, row: Row, schema: Schema) = {
    val ruleValue = rootPath.referenceValue(columnIndex, row, schema)

    val fileExists = ruleValue match {
      case Some(rp) => new FileSystem(rp, filePath, pathSubstitutions).exists
      case None =>   new FileSystem(filePath, pathSubstitutions).exists
    }

    fileExists
  }

  override def toError = s"""$ruleName""" + (if (rootPath == Literal(None)) "" else s"""(${rootPath.toError})""")
}

case class InRule(inValue: ArgProvider) extends Rule("in", inValue) {
  def valid(cellValue: String, columnDefinition: ColumnDefinition, columnIndex: Int, row: Row, schema: Schema): Boolean = {
    val ruleValue = inValue.referenceValue(columnIndex, row, schema)

    val (rv, cv) = if (columnDefinition.directives.contains(IgnoreCase())) (ruleValue.get.toLowerCase, cellValue.toLowerCase) else (ruleValue.get, cellValue)
    rv contains cv
  }
}

case class IsRule(isValue: ArgProvider) extends Rule("is", isValue) {
  def valid(cellValue: String, columnDefinition: ColumnDefinition, columnIndex: Int, row: Row, schema: Schema): Boolean = {
    val ruleValue = isValue.referenceValue(columnIndex, row, schema)

    val (rv, cv) = if (columnDefinition.directives.contains(IgnoreCase())) (ruleValue.get.toLowerCase, cellValue.toLowerCase) else (ruleValue.get, cellValue)
    cv == rv
  }
}

case class IsNotRule(isNotValue: ArgProvider) extends Rule("isNot", isNotValue) {
  def valid(cellValue: String, columnDefinition: ColumnDefinition, columnIndex: Int, row: Row, schema: Schema): Boolean = {
    val ruleValue = isNotValue.referenceValue(columnIndex, row, schema)

    val (rv, cv) = if (columnDefinition.directives.contains(IgnoreCase())) (ruleValue.get.toLowerCase, cellValue.toLowerCase) else (ruleValue.get, cellValue)
    cv != rv
  }
}

case class StartsRule(startsValue: ArgProvider) extends Rule("starts", startsValue) {
  def valid(cellValue: String, columnDefinition: ColumnDefinition, columnIndex: Int, row: Row, schema: Schema): Boolean = {
    val ruleValue = startsValue.referenceValue(columnIndex, row, schema)

    val (rv, cv) = if (columnDefinition.directives.contains(IgnoreCase())) (ruleValue.get.toLowerCase, cellValue.toLowerCase) else (ruleValue.get, cellValue)
    cv startsWith rv
  }
}

case class EndsRule(endsValue: ArgProvider) extends Rule("ends", endsValue) {
  def valid(cellValue: String, columnDefinition: ColumnDefinition, columnIndex: Int, row: Row, schema: Schema): Boolean = {
    val ruleValue = endsValue.referenceValue(columnIndex, row, schema)

    val (rv, cv) = if (columnDefinition.directives.contains(IgnoreCase())) (ruleValue.get.toLowerCase, cellValue.toLowerCase) else (ruleValue.get, cellValue)
    cv endsWith rv
  }
}

//case class UriRule() extends PatternRule("uri", UriRegex)
case class UriRule() extends Rule("uri") {
  def valid(cellValue: String, columnDefinition: ColumnDefinition, columnIndex: Int, row: Row, schema: Schema): Boolean = {
    try {
      val uri = new URI(cellValue)
      true
    } catch {
      case use: URISyntaxException => false
    }
  }
}

trait DateParser {
  def parse(dateStr: String): Try[DateTime]
}

object IsoDateTimeParser extends DateParser {
  def parse(dateStr: String): Try[DateTime] = Try(DateTime.parse(dateStr))
}

object XsdDateParser extends DateParser {
  val xsdDateFormatter = new DateTimeFormatterBuilder()
    .append(ISODateTimeFormat.dateElementParser)
    .appendOptional(
      new DateTimeFormatterBuilder()
        .appendTimeZoneOffset("Z", true, 2, 4).toParser
    ).toFormatter

  def parse(dateStr: String): Try[DateTime] = Try(DateTime.parse(dateStr, xsdDateFormatter))
}

object IsoTimeParser extends DateParser {
  def parse(dateStr: String): Try[DateTime] = Try(DateTime.parse(dateStr, ISODateTimeFormat.timeParser))
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

  def valid(cellValue: String, columnDefinition: ColumnDefinition, columnIndex: Int, row: Row, schema: Schema): Boolean = {
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

abstract class PatternRule(name: String, pattern: String) extends Rule(name) {
  def valid(cellValue: String, columnDefinition: ColumnDefinition, columnIndex: Int, row: Row, schema: Schema): Boolean = cellValue matches pattern
}

abstract class DateRule(name: String, dateRegex: String, dateParser: DateParser) extends PatternRule(name, dateRegex) {
  override def valid(cellValue: String, columnDefinition: ColumnDefinition, columnIndex: Int, row: Row, schema: Schema): Boolean = {
    super.valid(cellValue, columnDefinition, columnIndex, row, schema) match {
      case true => dateParser.parse(cellValue).isSuccess
      case _ => false
    }
  }
}

case class XsdDateTimeRule() extends DateRule("xDateTime", XsdDateTimeRegex, IsoDateTimeParser)

case class XsdDateTimeRangeRule(from: String, to: String) extends DateRangeRule("xDateTime", XsdDateTimeRegex, IsoDateTimeParser)

case class XsdDateRule() extends DateRule("xDate", XsdDateRegex, XsdDateParser)

case class XsdDateRangeRule(from: String, to: String) extends DateRangeRule("xDate",  XsdDateRegex, XsdDateParser)

case class UkDateRule() extends DateRule("ukDate", UkDateRegex, UkDateParser)

case class UkDateRangeRule(from: String, to: String) extends DateRangeRule("ukDate", UkDateRegex, UkDateParser)

case class XsdTimeRule() extends DateRule("xTime", XsdTimeRegex, IsoTimeParser)

case class XsdTimeRangeRule(from: String, to: String) extends DateRangeRule("xTime", XsdTimeRegex, IsoTimeParser)

case class PartUkDateRule() extends PatternRule("partUkDate", PartUkDateRegex)

case class Uuid4Rule() extends PatternRule("uuid4", Uuid4Regex)

case class PositiveIntegerRule() extends PatternRule("positiveInteger", PositiveIntegerRegex)

case class UniqueRule() extends Rule("unique") {
  val distinctValues = mutable.HashMap[String, Int]()

  override def evaluate(columnIndex: Int, row: Row, schema: Schema): RuleValidation[Any] = {
    val columnDefinition = schema.columnDefinitions(columnIndex)

    def originalValue: Option[String] = {
      val cellValue = cellValueCorrectCase
      if (distinctValues contains cellValue) Some(cellValue) else None
    }

    def cellValueCorrectCase = if (columnDefinition.directives contains IgnoreCase()) cellValue(columnIndex,row,schema).toLowerCase else cellValue(columnIndex,row,schema)

    originalValue match {
      case None => distinctValues.put(cellValueCorrectCase, row.lineNumber); true.successNel
      case Some(o) => {
        s"$toError fails for line: ${row.lineNumber}, column: ${columnDefinition.id}, ${toValueError(row,columnIndex)} (original at line: ${distinctValues(o)})".failNel[Any]
      }
    }
  }

  def valid(cellValue: String, columnDefinition: ColumnDefinition, columnIndex: Int, row: Row, schema: Schema): Boolean = {
    evaluate(columnIndex, row, schema) match {
      case FailureZ(_) => false
      case SuccessZ(_) => true
    }
  }
}

case class UniqueMultiRule( columns: List[String] ) extends Rule("unique(") {
  val SEPARATOR:Char = 0x07 // BEL
  val distinctValues = mutable.HashMap[String, Int]()

  override def evaluate(columnIndex: Int, row: Row, schema: Schema): RuleValidation[Any] = {
    val columnDefinition = schema.columnDefinitions(columnIndex)

    def secondaryValues: String =  columns.foldLeft(""){ case (s,c) => s + SEPARATOR + row.cells(columnNameToIndex(schema, c)).value }

    def uniqueString: String =  cellValue(columnIndex,row,schema) + SEPARATOR +  secondaryValues

    def originalValue: Option[String] = {
      val cellValue = cellValueCorrectCase
      if (distinctValues contains cellValue) Some(cellValue) else None
    }

    def cellValueCorrectCase = if (columnDefinition.directives contains IgnoreCase) uniqueString.toLowerCase else uniqueString

    originalValue match {
      case None => distinctValues.put(cellValueCorrectCase, row.lineNumber); true.successNel
      case Some(o) => {
        s"$toError ${columns.mkString("$", ", $", "")} ) fails for line: ${row.lineNumber}, column: ${columnDefinition.id}, ${toValueError(row,columnIndex)} (original at line: ${distinctValues(o)})".failNel[Any]
      }
    }
  }

  def valid(cellValue: String, columnDefinition: ColumnDefinition, columnIndex: Int, row: Row, schema: Schema): Boolean = {
    evaluate(columnIndex, row, schema) match {
      case FailureZ(_) => false
      case SuccessZ(_) => true
    }
  }
}

case class ChecksumRule(rootPath: ArgProvider, file: ArgProvider, algorithm: String, pathSubstitutions: List[(String,String)]) extends Rule("checksum", rootPath, file) with FileWildcardSearch[String] {
  def this(file: ArgProvider, algorithm: String, pathSubstitutions: List[(String,String)]) = this(Literal(None), file, algorithm, pathSubstitutions)
  def this(file: ArgProvider, algorithm: String) = this(Literal(None), file, algorithm, List[(String,String)]())

  override def evaluate(columnIndex: Int, row: Row, schema: Schema): RuleValidation[Any] = {
    val columnDefinition = schema.columnDefinitions(columnIndex)

    search(filename(columnIndex, row, schema)) match {
      case SuccessZ(hexValue: String) if hexValue == cellValue(columnIndex,row,schema) => true.successNel[String]
      case SuccessZ(hexValue: String) => s"$toError file ${'"'}${filename(columnIndex, row, schema)._1}${filename(columnIndex, row, schema)._2}${'"'} checksum match fails for line: ${row.lineNumber}, column: ${columnDefinition.id}, ${toValueError(row,columnIndex)}. Computed checksum value:${'"'}${hexValue}${'"'}".failNel[Any]
      case FailureZ(errMsg) => s"$toError ${errMsg.head} for line: ${row.lineNumber}, column: ${columnDefinition.id}, ${toValueError(row,columnIndex)}".failNel[Any]
    }
  }

  def valid(cellValue: String, columnDefinition: ColumnDefinition, columnIndex: Int, row: Row, schema: Schema): Boolean = {
    evaluate(columnIndex, row, schema) match {
      case FailureZ(_) => false
      case SuccessZ(_) => true
    }
  }

  override def toError = {
    if (rootPath.toError.isEmpty) s"""$ruleName(file(${file.toError}), "$algorithm")"""
    else s"""$ruleName(file(${rootPath.toError}, ${file.toError}), "$algorithm")"""
  }

  private def filename(columnIndex: Int, row: Row, schema: Schema): (String,String) = {
    val f = file.referenceValue(columnIndex, row, schema).get

    rootPath.referenceValue(columnIndex, row, schema) match {
      case None => ("",f)
      case Some(r: String) if r.endsWith("/") => (r, f)
      case Some(r) => (r + "/", f)
    }
  }

  def matchWildcardPaths(matchList: PathSet[Path],fullPath: String): ValidationNel[String, String] = matchList.size match {
    case 1 => calcChecksum(matchList.head.path)
    case 0 => s"""no files for $fullPath found""".failNel[String]
    case _ => s"""multiple files for $fullPath found""".failNel[String]
  }

  def matchSimplePath(fullPath: String): ValidationNel[String, String]  = calcChecksum(fullPath)

  def calcChecksum(file: String): ValidationNel[String, String] = {
    val digest = MessageDigest.getInstance(algorithm)

    FileSystem.createFile(file) match {
      case scala.util.Success(f) =>
        val fileBuffer = new BufferedInputStream( new FileInputStream( f) )
        Stream.continually(fileBuffer.read).takeWhile(-1 !=).map(_.toByte).foreach( digest.update(_))
        fileBuffer.close()
        hexEncode(digest.digest).successNel[String]

      case scala.util.Failure(_) => s"""file '$file' not found""".failNel[String]
    }
  }

  private def hexEncode(in: Array[Byte]): String = {
    val sb = new StringBuilder
    val len = in.length

    def addDigit(in: Array[Byte], pos: Int, len: Int, sb: StringBuilder) {
      if (pos < len) {
        val b: Int = in(pos)
        val msb = (b & 0xf0) >> 4
        val lsb = (b & 0x0f)
        sb.append((if (msb < 10) ('0' + msb).asInstanceOf[Char] else ('a' + (msb - 10)).asInstanceOf[Char]))
        sb.append((if (lsb < 10) ('0' + lsb).asInstanceOf[Char] else ('a' + (lsb - 10)).asInstanceOf[Char]))

        addDigit(in, pos + 1, len, sb)
      }
    }

    addDigit(in, 0, len, sb)
    sb.toString()
  }
}

case class FileCountRule(rootPath: ArgProvider, file: ArgProvider, pathSubstitutions: List[SubstitutePath] = List.empty) extends Rule("fileCount", rootPath, file) with FileWildcardSearch[Int] {
  def this(file: ArgProvider, pathSubstitutions: List[SubstitutePath] = List.empty) = this(Literal(None), file, pathSubstitutions)
  def this(rootPath: Literal, file: Literal) = this(rootPath, file,  List.empty)

  def valid(cellValue: String, columnDefinition: ColumnDefinition, columnIndex: Int, row: Row, schema: Schema): Boolean = {
    evaluate(columnIndex, row, schema) match {
      case FailureZ(_) => false
      case SuccessZ(_) => true
    }
  }

  override def evaluate(columnIndex: Int, row: Row, schema: Schema): RuleValidation[Any] = {
    val columnDefinition = schema.columnDefinitions(columnIndex)

    Try(cellValue(columnIndex,row,schema).toInt) match {
      case scala.util.Success(cellCount) =>
        search(filename(columnIndex, row, schema)) match {
          case SuccessZ(count: Int) if count == cellCount => true.successNel[String]
          case SuccessZ(count: Int) => s"$toError found $count file(s) for line: ${row.lineNumber}, column: ${columnDefinition.id}, ${toValueError(row,columnIndex)}".failNel[Any]
          case FailureZ(errMsg) => s"$toError ${errMsg.head} for line: ${row.lineNumber}, column: ${columnDefinition.id}, ${toValueError(row,columnIndex)}".failNel[Any]
        }

      case scala.util.Failure(_) =>  s"$toError '${cellValue(columnIndex,row,schema)}' is not a number for line: ${row.lineNumber}, column: ${columnDefinition.id}, ${toValueError(row,columnIndex)}".failNel[Any]
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
  abstract class TypedPath {

    def path : String

    val separator = UNIX_FILE_SEPARATOR
    //private lazy val p = Path.fromString(path)

    def name = path.lastIndexOf(separator) match {
      case -1 =>
        path
      case index =>
        path.substring(index + 1)
    }
    //def hasParent = !p.parent.isEmpty
    //protected def parentPath = p.parent.get.path
    def hasParent = parentPath.nonEmpty
    private lazy val parentPath : Option[String] = {
      path.lastIndexOf(separator) match {
        case -1 =>
          None
        case index =>
          Some(path.substring(0, index))
      }
    }
    def parent : Option[TypedPath] = parentPath match {
      case Some(pp) => Some(construct(pp))
      case _ => None
    }
    protected def construct(p: String) : TypedPath
    def thisFolder : TypedPath = construct(".")
    def toString : String
    protected lazy val isWindowsPlatform = sys.props("os.name").toLowerCase.startsWith("win")
    def toPlatform : TypedPath
  }

  object TypedPath {

    //extracts the scheme from the file:/c:/ or file:/// or file:///c:/ part of a URI
    lazy val fileUriPattern = Pattern.compile("((file:///)[a-zA-Z]+:/|(file://)/|(file:/)[a-zA-Z]+:/)(.*)")
    //extracts the path from the URI
    lazy val fileUriPathPattern = Pattern.compile("file:///([a-zA-Z]+:/.*)|file://(/.*)|file:/([a-zA-Z]+:/.*)")

    def apply(path : String) : TypedPath = {
      val fileUriMatcher = fileUriPattern.matcher(path)
      if(fileUriMatcher.matches) {
       val fileUriPathMatcher = fileUriPathPattern.matcher(path)
        new FileUriPath(fileUriMatcher.replaceFirst("$2$3$4"), fileUriPathMatcher.replaceFirst("$1$2$3"))
      } else if(path.contains(UNIX_FILE_SEPARATOR) && !path.contains(WINDOWS_FILE_SEPARATOR)) {
        new UnixPath(path)
      } else {
        new WindowsPath(path)
      }
    }
  }

  case class FileUriPath(uriPrefix : String, path: String) extends TypedPath {
    def toURI : URI = new URI(uriPrefix + FileSystem.file2PlatformIndependent(path))
    override def toString : String = toURI.toString
    override protected def construct(p : String) = new FileUriPath(uriPrefix, p);
    override def toPlatform = this
  }
  case class WindowsPath(path : String) extends TypedPath {
    override val separator = WINDOWS_FILE_SEPARATOR
    override def toString = FileSystem.file2WindowsPlatform(path)
    override protected def construct(p : String) = new WindowsPath(p)
    override def toPlatform = if(isWindowsPlatform) this else new UnixPath(FileSystem.file2UnixPlatform(path))
  }
  case class UnixPath(path : String) extends TypedPath {
    override def toString = FileSystem.file2UnixPlatform(path)
    override protected def construct(p: String) = new UnixPath(p)
    override def toPlatform = if(isWindowsPlatform) new WindowsPath(FileSystem.file2WindowsPlatform(path)) else this
  }

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
      val fullPath = new FileSystem( None, filePaths._1 + filePaths._2, pathSubstitutions).expandBasePath
      val (basePath, matchPath) = findBase(fullPath)

      val path: Option[Path] = {
        FileSystem.createFile( basePath.toPlatform.toString ) match {
          case scala.util.Success(f) => Some(Path(f))
          case scala.util.Failure(_) => None
        }
      }

      def pathString = s"${filePaths._1} (localfile: $fullPath)"

      def findMatches(wc: (Path, String) => PathSet[Path] ): ValidationNel[String, T] = {
        path match {
          case Some(p) =>  matchWildcardPaths( wc(p, matchPath ), fullPath )
          case None => "no file".failNel[T]
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
        s"""incorrect basepath $pathString found""".failNel[T]
      } else if(wildcardNotInRoot ) {
        s"""root $pathString should not contain wildcards""".failNel[T]
      } else if(matchUsesWildDirectory) {
        findMatches(wildcardPath)
      } else if(matchUsesWildFiles) {
        findMatches(wildcardFile)
      } else if(!fileExists) {
        s"""file "$fullPath" not found""".failNel[T]
      } else {
        matchSimplePath(basePath + System.getProperty("file.separator") + matchPath)
      }
    } catch {
      case err:Throwable =>
        err.getMessage.failNel[T]
    }
  }
}

case class RangeRule(min: BigDecimal, max: BigDecimal) extends Rule("range") {
  def valid(cellValue: String, columnDefinition: ColumnDefinition, columnIndex: Int, row: Row, schema: Schema): Boolean = {

    Try[BigDecimal]( BigDecimal(cellValue)) match {
      case scala.util.Success(callDecimal) => if (callDecimal >= min && callDecimal <= max  ) true  else false
      case _ => false
     }
  }

  override def toError = s"""$ruleName($min,$max)"""
}

case class LengthRule(from: Option[String], to: String) extends Rule("length") {

  def toValue: Int = if (to == "*") Int.MaxValue else to.toInt
  def fromValue: Int =  if (from.get == "*") 0 else from.get.toInt

  def valid(cellValue: String, columnDefinition: ColumnDefinition, columnIndex: Int, row: Row, schema: Schema): Boolean = {
    val cellLen = cellValue.length

    from match {
      case None => if ( to=="*") true else cellLen == to.toInt
      case Some(_) => cellLen >= fromValue && cellLen <= toValue
    }
  }

  override def toError = if(from.isDefined) s"""$ruleName(${from.get},$to)""" else s"""$ruleName($to)"""
}

case class AndRule(left: Rule, right: Rule) extends Rule("and") {
  override def evaluate(columnIndex: Int, row: Row, schema: Schema): RuleValidation[Any] = {
    left.evaluate(columnIndex, row, schema) match {
      case s @ FailureZ(_) => fail(columnIndex, row, schema)

      case SuccessZ(_) => right.evaluate(columnIndex, row, schema) match {
        case s @ SuccessZ(_) => s
        case FailureZ(_) => fail(columnIndex, row, schema)
      }
    }
  }

  def valid(cellValue: String, columnDefinition: ColumnDefinition, columnIndex: Int, row: Row, schema: Schema): Boolean = {
    evaluate(columnIndex, row, schema) match {
      case FailureZ(_) => false
      case SuccessZ(_) => true
    }
  }

  override def toError = s"""${left.toError} $ruleName ${right.toError}"""
}

object FileSystem {
  def createFile(filename: String): Try[File] =  Try{ if( filename.startsWith("file:")) new File( new URI(file2PlatformIndependent(filename))) else  new File( filename )}

  def replaceSpaces(file: String): String = file.replace(" ", "%20")

  def file2PlatformIndependent(file: String): String =
    file.replaceAll("""([^\\]?)\\([^\\]?)""", "$1/$2")

  def file2UnixPlatform(file: String) : String = file2PlatformIndependent(file)

  def file2WindowsPlatform(file: String) : String =
    file.replaceAll("([^/]?)/([^/]?)", """$1\\$2""")

  def convertPath2Platform(filename: String): String = {
    if ( filename.startsWith("file:/"))  replaceSpaces(filename) else file2PlatformIndependent( filename )
  }
}

case class FileSystem(basePath: Option[String], file: String, pathSubstitutions: List[(String,String)] ) {

  def this( root:String, file: String, pathSubstitutions: List[(String,String)] ) = this( Some(root), file, pathSubstitutions)

  def this( file: String, pathSubstitutions: List[(String,String)]) = this(None, file, pathSubstitutions)

  val separator: Char = FILE_SEPARATOR

  private def substitutePath(filename: String): String = {
    val x = {
      pathSubstitutions.filter {
        case (subFrom, _) => filename.contains(subFrom)
      }.map {
        case (subFrom, subTo) => filename.replaceFirst(Matcher.quoteReplacement(subFrom), Matcher.quoteReplacement(FileSystem.file2PlatformIndependent(subTo)))
      }
    }
    if(x.isEmpty)
      filename
    else
      x.head
  }

  def jointPath: String = {
    val uri_sep: Char = URI_PATH_SEPARATOR

    basePath match {
      case Some(bp) =>

        if (bp.length > 0 && bp.last != uri_sep && file.head != uri_sep) {
          bp + uri_sep + file
        } else if (bp.length > 0 && bp.last == uri_sep && file.head == uri_sep) {
          bp + file.tail
        } else {
          bp + file
        }

      case None => file
    }
  }

  def exists: Boolean = {
    FileSystem.createFile(FileSystem.convertPath2Platform(substitutePath(jointPath))) match {
      case scala.util.Success(f) => f.exists
      case scala.util.Failure(_) => false
    }
  }

  def expandBasePath: String = {
    if( basePath.isEmpty || basePath.getOrElse("") == "")
      FileSystem.file2PlatformIndependent(substitutePath(file))
    else
      FileSystem.file2PlatformIndependent(substitutePath(jointPath))
  }
}