package uk.gov.tna.dri.schema

import scalax.file.{PathSet, Path}
import scalaz.Scalaz._
import java.io.{BufferedInputStream, FileInputStream, File}
import scala.util.parsing.input.Positional
import scala.collection.mutable
import java.security.MessageDigest
import scalaz.{Failure,Success}
import uk.gov.tna.dri.metadata.Row
import util.Try
import annotation.tailrec
import java.net.URI
import org.joda.time.{Interval, LocalTime, DateTime}
import org.joda.time.format.DateTimeFormat

abstract class Rule(val name: String, val argProviders: ArgProvider*) extends Positional {

  type RuleValidation[A] = ValidationNEL[String, A]

  def evaluate(columnIndex: Int, row: Row, schema: Schema): RuleValidation[Any] = {
    val cellValue = row.cells(columnIndex).value
    if (valid(cellValue, schema.columnDefinitions(columnIndex), columnIndex, row, schema)) true.successNel[String] else fail(columnIndex, row, schema)
  }

  def valid(cellValue: String, columnDefinition: ColumnDefinition, columnIndex: Int, row: Row, schema: Schema): Boolean

  def fail(columnIndex: Int, row: Row, schema: Schema): RuleValidation[Any] = {
    val columnDefinition = schema.columnDefinitions(columnIndex)
    s"$toError fails for line: ${row.lineNumber}, column: ${columnDefinition.id}, value: ${'"'}${row.cells(columnIndex).value}${'"'}".failNel[Any]
  }

  def toError = s"""$name""" + (if (argProviders.isEmpty) "" else "(" + argProviders.foldLeft("")((a, b) => (if (a.isEmpty) "" else a + ", ") + b.toError) + ")")
}

case class OrRule(left: Rule, right: Rule) extends Rule("or") {
  override def evaluate(columnIndex: Int, row: Row, schema: Schema): RuleValidation[Any] = {
    left.evaluate(columnIndex, row, schema) match {
      case s @ Success(_) => s
      case Failure(_) => right.evaluate(columnIndex, row, schema) match {
        case s @ Success(_) => s
        case Failure(_) => fail(columnIndex, row, schema)
      }
    }
  }

  def valid(cellValue: String, columnDefinition: ColumnDefinition, columnIndex: Int, row: Row, schema: Schema) = true

  override def toError = s"""${left.toError} $name ${right.toError}"""
}

case class ParenthesesRule(rules: List[Rule]) extends Rule("parentheses") {

  override def evaluate(columnIndex: Int, row: Row, schema: Schema): RuleValidation[Any] = {
    val v = for (rule <- rules) yield {
      rule.evaluate(columnIndex, row, schema)
    }

    v.sequence[RuleValidation, Any]
  }

  def valid(cellValue: String, columnDefinition: ColumnDefinition, columnIndex: Int, row: Row, schema: Schema) = true

  override def toError = {
    val paramErrs = rules.map( _.toError).mkString(" ")
    s"""($paramErrs)""" + (if (argProviders.isEmpty) "" else "(" + argProviders.foldLeft("")((a, b) => (if (a.isEmpty) "" else a + ", ") + b.toError) + ")")
  }

}

case class IfRule(condition: Rule, rules: List[Rule], elseRules: Option[List[Rule]]) extends Rule("if") {

  override def evaluate(columnIndex: Int, row: Row, schema: Schema): RuleValidation[Any] = {
    val cellValue = row.cells(columnIndex).value

    val v = if (condition.valid(cellValue, schema.columnDefinitions(columnIndex), columnIndex, row, schema)) {
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

  def valid(cellValue: String, columnDefinition: ColumnDefinition, columnIndex: Int, row: Row, schema: Schema) = true

  override def toError = {
    val paramErrs = rules.map( _.toError).mkString(" ")
    s"""($paramErrs)""" + (if (argProviders.isEmpty) "" else "(" + argProviders.foldLeft("")((a, b) => (if (a.isEmpty) "" else a + ", ") + b.toError) + ")")
  }

}

case class RegexRule(regex: String) extends Rule("regex") {
  def valid(cellValue: String, columnDefinition: ColumnDefinition, columnIndex: Int, row: Row, schema: Schema) = {

    val regexp = if (columnDefinition.directives.contains(IgnoreCase())) "(?i)" + regex else regex
    cellValue matches regexp
  }

  override def toError = {
    s"""$name("$regex")"""
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

  override def toError = s"""$name""" + (if (rootPath == Literal(None)) "" else s"""(${rootPath.toError})""")
}

case class InRule(inValue: ArgProvider) extends Rule("in", inValue) {
  def valid(cellValue: String, columnDefinition: ColumnDefinition, columnIndex: Int, row: Row, schema: Schema): Boolean = {
    val ruleValue = inValue.referenceValue(columnIndex, row, schema)

    val (rv, cv) = if (columnDefinition.directives.contains(IgnoreCase())) (ruleValue.get.toLowerCase, cellValue.toLowerCase) else (ruleValue.get, cellValue)
    rv contains cv
  }
}

case class IsRule(isValue: ArgProvider) extends Rule("is", isValue) {
  def valid(cellValue: String, columnDefinition: ColumnDefinition, columnIndex: Int, row: Row, schema: Schema) = {
    val ruleValue = isValue.referenceValue(columnIndex, row, schema)

    val (rv, cv) = if (columnDefinition.directives.contains(IgnoreCase())) (ruleValue.get.toLowerCase, cellValue.toLowerCase) else (ruleValue.get, cellValue)
    cv == rv
  }
}

case class IsNotRule(isNotValue: ArgProvider) extends Rule("isNot", isNotValue) {
  def valid(cellValue: String, columnDefinition: ColumnDefinition, columnIndex: Int, row: Row, schema: Schema) = {
    val ruleValue = isNotValue.referenceValue(columnIndex, row, schema)

    val (rv, cv) = if (columnDefinition.directives.contains(IgnoreCase())) (ruleValue.get.toLowerCase, cellValue.toLowerCase) else (ruleValue.get, cellValue)
    cv != rv
  }
}

case class StartsRule(startsValue: ArgProvider) extends Rule("starts", startsValue) {
  def valid(cellValue: String, columnDefinition: ColumnDefinition, columnIndex: Int, row: Row, schema: Schema) = {
    val ruleValue = startsValue.referenceValue(columnIndex, row, schema)

    val (rv, cv) = if (columnDefinition.directives.contains(IgnoreCase())) (ruleValue.get.toLowerCase, cellValue.toLowerCase) else (ruleValue.get, cellValue)
    cv startsWith rv
  }
}

case class EndsRule(endsValue: ArgProvider) extends Rule("ends", endsValue) {
  def valid(cellValue: String, columnDefinition: ColumnDefinition, columnIndex: Int, row: Row, schema: Schema) = {
    val ruleValue = endsValue.referenceValue(columnIndex, row, schema)

    val (rv, cv) = if (columnDefinition.directives.contains(IgnoreCase())) (ruleValue.get.toLowerCase, cellValue.toLowerCase) else (ruleValue.get, cellValue)
    cv endsWith rv
  }
}

case class UriRule() extends Rule("uri") {
  val uriRegex = "http://datagov.nationalarchives.gov.uk/66/WO/409/[0-9]+/[0-9]+/" + Uuid4Regex
  def valid(cellValue: String, columnDefinition: ColumnDefinition, columnIndex: Int, row: Row, schema: Schema) = cellValue matches uriRegex
}

abstract class DateRangeRule(override val name: String) extends Rule(name) {

  val dateRegex: String
  val fromDate: Try[DateTime]
  val toDate: Try[DateTime]
  val from: String
  val to: String

  def valid(cellValue: String, columnDefinition: ColumnDefinition, columnIndex: Int, row: Row, schema: Schema): Boolean = {
    cellValue matches dateRegex match {
      case true => {
        val inRange = for ( frmDt <- fromDate; toDt <-toDate; cellDt <- calcCellValueDate(cellValue)) yield {
          val interval = new Interval(frmDt,toDt.plusMillis(1))
          interval.contains(cellDt)
        }
        inRange.getOrElse(false)
      }
      case _ => false
    }
  }

  override def toError = {
    s"""$name("$from, $to")"""
  }

  def calcCellValueDate(cellValue: String) = Try(DateTime.parse(cellValue))
}

abstract class DateRule(override val name: String) extends Rule(name) {
  val dateRegex: String

  def valid(cellValue: String, columnDefinition: ColumnDefinition, columnIndex: Int, row: Row, schema: Schema): Boolean = {
    cellValue matches dateRegex match {
      case true => calcCellValueDate(cellValue).isSuccess
      case _ => false
    }
  }

  def calcCellValueDate(cellValue: String) = Try(DateTime.parse(cellValue))
}

case class XsdDateTimeRule() extends DateRule("xDateTime") {
  val dateRegex = XsdDateTimeRegex
}

case class XsdDateTimeRangeRule(from: String, to: String) extends DateRangeRule("xDateTime")  {
  val dateRegex = XsdDateTimeRegex
  lazy val fromDate = Try(DateTime.parse(from))
  lazy val toDate = Try(DateTime.parse(to))
}

case class XsdDateRule() extends DateRule("xDate") {
  val dateRegex = XsdDateRegex
}

case class XsdDateRangeRule(from: String, to: String) extends DateRangeRule("xDate") {
  val dateRegex = XsdDateRegex
  lazy val fromDate = Try(DateTime.parse(from))
  lazy val toDate = Try(DateTime.parse(to))
}

case class UkDateRule() extends DateRule("ukDate") {
  val dateRegex = UkDateRegex
  val fmt = DateTimeFormat.forPattern(UkDateFormat)

  override def calcCellValueDate(cellValue: String) = Try(fmt.parseDateTime(cellValue))
}

case class UkDateRangeRule(from: String, to: String) extends DateRangeRule("ukDate") {
  val dateRegex = UkDateRegex
  val fmt = DateTimeFormat.forPattern(UkDateFormat)
  lazy val fromDate = Try(fmt.parseDateTime(from))
  lazy val toDate = Try(fmt.parseDateTime(to))

  override def calcCellValueDate(cellValue: String) = Try(fmt.parseDateTime(cellValue))
}

case class XsdTimeRule() extends DateRule("xTime") {
  val dateRegex = XsdTimeRegex

  override def calcCellValueDate(cellValue: String) = Try(LocalTime.parse(cellValue).toDateTimeToday)
}

case class XsdTimeRangeRule(from: String, to: String) extends DateRangeRule("xTime") {
  val dateRegex = XsdTimeRegex
  lazy val fromDate = Try(LocalTime.parse(from).toDateTimeToday)
  lazy val toDate = Try(LocalTime.parse(to).toDateTimeToday)

  override def calcCellValueDate(cellValue: String) = Try(LocalTime.parse(cellValue).toDateTimeToday)
}

case class Uuid4Rule() extends Rule("uuid4") {
  def valid(cellValue: String, columnDefinition: ColumnDefinition, columnIndex: Int, row: Row, schema: Schema) = cellValue matches Uuid4Regex
}

case class PositiveIntegerRule() extends Rule("positiveInteger") {
  def valid(cellValue: String, columnDefinition: ColumnDefinition, columnIndex: Int, row: Row, schema: Schema) = cellValue matches PositiveIntegerRegex
}

case class UniqueRule() extends Rule("unique") {
  val distinctValues = mutable.HashMap[String, Int]()

  override def evaluate(columnIndex: Int, row: Row, schema: Schema): RuleValidation[Any] = {
    val cellValue = row.cells(columnIndex).value
    val columnDefinition = schema.columnDefinitions(columnIndex)

    def originalValue: Option[String] = {
      val cellValue = cellValueCorrectCase
      if (distinctValues contains cellValue) Some(cellValue) else None
    }

    def cellValueCorrectCase = if (columnDefinition.directives contains IgnoreCase()) cellValue.toLowerCase else cellValue

    originalValue match {
      case None => distinctValues.put(cellValueCorrectCase, row.lineNumber); true.successNel
      case Some(o) => {
        s"$toError fails for line: ${row.lineNumber}, column: ${columnDefinition.id}, value: ${'"'}${row.cells(columnIndex).value}${'"'} (original at line: ${distinctValues(o)})".failNel[Any]
      }
    }
  }

  def valid(cellValue: String, columnDefinition: ColumnDefinition, columnIndex: Int, row: Row, schema: Schema) = true
}

case class UniqueMultiRule( columns: List[String] ) extends Rule("unique(") {
  val SEPARATOR:Char = 0x07 // BEL
  val distinctValues = mutable.HashMap[String, Int]()

  override def evaluate(columnIndex: Int, row: Row, schema: Schema): RuleValidation[Any] = {
    val cellValue = row.cells(columnIndex).value
    val columnDefinition = schema.columnDefinitions(columnIndex)

    def columnName2Index( name: String): Int = schema.columnDefinitions.zipWithIndex.filter{ case (c,i) => c.id == name}.head._2

    def secondaryValues: String =  columns.foldLeft(""){ case (s,c) => s + SEPARATOR + row.cells(columnName2Index(c)).value }

    def uniqueString: String =  cellValue + SEPARATOR +  secondaryValues

    def originalValue: Option[String] = {
      val cellValue = cellValueCorrectCase
      if (distinctValues contains cellValue) Some(cellValue) else None
    }

    def cellValueCorrectCase = if (columnDefinition.directives contains IgnoreCase) uniqueString.toLowerCase else uniqueString

    originalValue match {
      case None => distinctValues.put(cellValueCorrectCase, row.lineNumber); true.successNel
      case Some(o) => {
        s"$toError ${columns.mkString("$", ", $", "")} ) fails for line: ${row.lineNumber}, column: ${columnDefinition.id}, value: ${'"'}${row.cells(columnIndex).value}${'"'} (original at line: ${distinctValues(o)})".failNel[Any]
      }
    }
  }

  def valid(cellValue: String, columnDefinition: ColumnDefinition, columnIndex: Int, row: Row, schema: Schema) = true
}

case class ChecksumRule(rootPath: ArgProvider, file: ArgProvider, algorithm: String, pathSubstitutions: List[(String,String)]) extends Rule("checksum", rootPath, file) with FileWildcardSearch[String] {
  def this(file: ArgProvider, algorithm: String, pathSubstitutions: List[(String,String)]) = this(Literal(None), file, algorithm, pathSubstitutions)
  def this(file: ArgProvider, algorithm: String) = this(Literal(None), file, algorithm, List[(String,String)]())

  override def evaluate(columnIndex: Int, row: Row, schema: Schema): RuleValidation[Any] = {
    val cellValue = row.cells(columnIndex).value
    val columnDefinition = schema.columnDefinitions(columnIndex)

    search(filename(columnIndex, row, schema)) match {
      case Success(hexValue: String) if hexValue == cellValue => true.successNel[String]
      case Success(hexValue: String) => s"$toError checksum match fails for line: ${row.lineNumber}, column: ${columnDefinition.id}, value: ${'"'}${row.cells(columnIndex).value}${'"'}".failNel[Any]
      case Failure(errMsg) => s"$toError ${errMsg.head} for line: ${row.lineNumber}, column: ${columnDefinition.id}, value: ${'"'}${row.cells(columnIndex).value}${'"'}".failNel[Any]
    }
  }

  def valid(cellValue: String, columnDefinition: ColumnDefinition, columnIndex: Int, row: Row, schema: Schema) = true

  override def toError = {
    if (rootPath.toError.isEmpty) s"""$name(file(${file.toError}), "$algorithm")"""
    else s"""$name(file(${rootPath.toError}, ${file.toError}), "$algorithm")"""
  }

  private def filename(columnIndex: Int, row: Row, schema: Schema): (String,String) = {
    val f = file.referenceValue(columnIndex, row, schema).get

    rootPath.referenceValue(columnIndex, row, schema) match {
      case None => ("",f)
      case Some(r: String) if r.endsWith("/") => (r, f)
      case Some(r) => (r + "/", f)
    }
  }

  def matchWildcardPaths(matchList: PathSet[Path],fullPath: String): ValidationNEL[String, String] = matchList.size match {
    case 1 => calcChecksum(matchList.head.path)//.successNel[String]
    case 0 => s"""no files for $fullPath found""".failNel[String]
    case _ => s"""multiple files for $fullPath found""".failNel[String]
  }

  def matchSimplePath(fullPath: String): ValidationNEL[String, String]  = calcChecksum(fullPath)//.successNel[String]

  def calcChecksum(file: String): ValidationNEL[String, String] = {
    val digest = MessageDigest.getInstance(algorithm)

    FileSystem.createFile(file) match {
      case scala.util.Success(f) =>
        val fileBuffer = new BufferedInputStream( new FileInputStream( f) )
        Stream.continually(fileBuffer.read).takeWhile(-1 !=).map(_.toByte).foreach( digest.update(_))
        fileBuffer.close()
        hexEncode(digest.digest).successNel[String]
      case scala.util.Failure(_) => "file not fund".failNel[String]
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

case class FileCountRule(rootPath: ArgProvider, file: ArgProvider, pathSubstitutions: List[(String,String)] = List.empty) extends Rule("fileCount", rootPath, file) with FileWildcardSearch[Int] {
  def this(file: ArgProvider, pathSubstitutions: List[(String,String)] = List.empty) = this(Literal(None), file, pathSubstitutions)

  def valid(cellValue: String, columnDefinition: ColumnDefinition, columnIndex: Int, row: Row, schema: Schema) = true

  override def evaluate(columnIndex: Int, row: Row, schema: Schema): RuleValidation[Any] = {
    val cellValue = row.cells(columnIndex).value
    val columnDefinition = schema.columnDefinitions(columnIndex)

    Try(cellValue.toInt) match {
      case scala.util.Success(cellCount) =>
        search(filename(columnIndex, row, schema)) match {
          case Success(count: Int) if count == cellCount => true.successNel[String]
          case Success(count: Int) => s"$toError found $count file(s) for line: ${row.lineNumber}, column: ${columnDefinition.id}, value: ${'"'}${row.cells(columnIndex).value}${'"'}".failNel[Any]
          case Failure(errMsg) => s"$toError ${errMsg.head} for line: ${row.lineNumber}, column: ${columnDefinition.id}, value: ${'"'}${row.cells(columnIndex).value}${'"'}".failNel[Any]
        }
      case scala.util.Failure(_) =>  s"$toError '$cellValue' is not a number for line: ${row.lineNumber}, column: ${columnDefinition.id}, value: ${'"'}${row.cells(columnIndex).value}${'"'}".failNel[Any]
    }
  }

  override def toError = {
    if (rootPath.toError.isEmpty) s"""$name(file(${file.toError}))"""
    else s"""$name(file(${rootPath.toError}, ${file.toError}))"""
  }

  private def filename(columnIndex: Int, row: Row, schema: Schema): (String,String) = {  // return (base,path)
    val f = file.referenceValue(columnIndex, row, schema).get

    rootPath.referenceValue(columnIndex, row, schema) match {
      case None => ("",f)
      case Some(r: String) if r.endsWith("/") => (r, f)
      case Some(r) => (r + "/", f)
    }
  }

  def matchWildcardPaths(matchList: PathSet[Path],fullPath: String): ValidationNEL[String, Int] = matchList.size.successNel[String]

  def matchSimplePath(fullPath: String): ValidationNEL[String, Int]  = 1.successNel[String]  // file found so ok
}

trait FileWildcardSearch[T] {
  val pathSubstitutions: List[(String,String)]
  def matchWildcardPaths(matchList: PathSet[Path],fullPath: String): ValidationNEL[String, T]
  def matchSimplePath(fullPath: String): ValidationNEL[String, T]

  val wildcardPath = (p: Path, matchPath: String) => p.descendants( p.matcher( matchPath))
  val wildcardFile = (p: Path, matchPath: String) => p.children( p.matcher( "**/" +matchPath))

  def findBase(path:String): (String, String) = {

    @tailrec
    def findBaseRecur(p: String, f: String): (String,String) = {
      if (p.contains("*")) findBaseRecur(Path.fromString(p).parent.get.path,  Path.fromString(p).name + "/" + f)
      else (p,f)
    }

    def spaces2twenty( file: String):String = file.replace(" ", "%20")

    if (path.startsWith("file://"))  {
      val pathURI = Path(new URI(spaces2twenty(path))).get
      findBaseRecur("file://" + pathURI.parent.get.path, pathURI.name)
    } else if (Path.fromString(path).parent.isEmpty) ("./", path) else findBaseRecur(Path.fromString(path).parent.get.path, Path.fromString(path).name)
  }

  def search(filePaths: (String, String) ): ValidationNEL[String, T] = {
    try{
      val fullPath = new FileSystem( None, filePaths._1 + filePaths._2, pathSubstitutions).expandBasePath
      val (basePath,matchPath ) = findBase(fullPath)

      val path:Path = {
        val file = FileSystem.createFile( basePath ) match {
          case scala.util.Success(p) => p
          case scala.util.Failure(_) => new File(".")
        }
        Path( file )
      }

      def pathString = s"${filePaths._1} (localfile: $fullPath)"

      def findMatches(wc: (Path, String) => PathSet[Path] ): ValidationNEL[String, T] = matchWildcardPaths( wc(path,matchPath ), fullPath )

      def basePathExists:Boolean =   filePaths._1.length>0 && (!(FileSystem.createFile( basePath ) match {
        case scala.util.Success(f) =>   f.exists
        case scala.util.Failure(_) => false
      }))

      def wildcardNotInRoot:Boolean = filePaths._1.contains("*")

      def matchUsesWildDirectory:Boolean = matchPath.contains("**")

      def matchUsesWildFiles:Boolean = matchPath.contains("*")

      def fileExists:Boolean = {
        val path = basePath+System.getProperty("file.separator")+matchPath
        FileSystem.createFile( path ) match {
          case scala.util.Success(file) =>   file.exists
          case scala.util.Failure(_) => false
        }
      }

      if ( basePathExists) s"""incorrect basepath $pathString found""".failNel[T]
      else if (wildcardNotInRoot ) s"""root $pathString should not contain wildcards""".failNel[T]
      else if (matchUsesWildDirectory) findMatches(wildcardPath)
      else if (matchUsesWildFiles)  findMatches(wildcardFile)
      else if (!fileExists)  s"""file "$fullPath" not found""".failNel[T]
      else matchSimplePath(basePath+System.getProperty("file.separator")+matchPath)
    } catch {
      case err:Throwable => err.getMessage.failNel[T]
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

  override def toError = s"""$name($min,$max)"""
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

  override def toError = if(from.isDefined) s"""$name(${from.get},$to)""" else s"""$name($to)"""
}

case class AndRule(left: Rule, right: Rule) extends Rule("and") {
  override def evaluate(columnIndex: Int, row: Row, schema: Schema): RuleValidation[Any] = {
    left.evaluate(columnIndex, row, schema) match {
      case s @ Failure(_) => fail(columnIndex, row, schema) // s
      case Success(_) => right.evaluate(columnIndex, row, schema) match {
        case s @ Success(_) => s
        case Failure(_) => fail(columnIndex, row, schema)
      }
    }
  }

  def valid(cellValue: String, columnDefinition: ColumnDefinition, columnIndex: Int, row: Row, schema: Schema) = true

  override def toError = s"""${left.toError} $name ${right.toError}"""
}

object FileSystem {
  def createFile( filename:String): Try[File] =  Try{ if( filename.startsWith("file:")) new File( new URI(filename)) else  new File( filename )}

  def spaces2twenty( file: String):String = file.replace(" ", "%20")

  private def file2PlatformDependent( file: String): String =
    if ( System.getProperty("file.separator") == "/" ) file.replace('\\', '/')
    else file.replace('/', '\\')

  def convertPath2Platform(filename: String): Try[File] = {
    val file = if ( filename.startsWith("file://"))  spaces2twenty(filename) else file2PlatformDependent( filename )
    createFile( file )
  }

}

case class FileSystem(basePath: Option[String], file: String, pathSubstitutions: List[(String,String)] ) {

  def this( root:String, file: String, pathSubstitutions: List[(String,String)] ) = this( Some(root), file, pathSubstitutions)

  def this( file: String, pathSubstitutions: List[(String,String)]) = this(None, file, pathSubstitutions)

  private def substitutePath(  filename: String): String = {
    val x = pathSubstitutions.filter(arg => filename.contains(arg._1)).map( arg => filename.replaceFirst(arg._1,arg._2) )
    if (x.isEmpty) filename else x.head
  }

  def jointPath: String = {
    val fs: Char = System.getProperty("file.separator").head

    basePath match {
      case Some(bp) => if (bp.length > 0 && bp.last != fs && file.head != fs) bp + fs + file
      else if (bp.length > 0 && bp.last == fs && file.head == fs) bp + file.tail
      else bp + file
      case None => file
    }
  }

  def exists: Boolean = {
    FileSystem.convertPath2Platform( substitutePath(jointPath)) match {
      case scala.util.Success(f) => f.exists
      case scala.util.Failure(_) => false
    }
  }

  def expandBasePath: String = {
    if ( basePath.isEmpty || basePath.getOrElse("") == "")  FileSystem.file2PlatformDependent(substitutePath(file))
    else FileSystem.file2PlatformDependent(substitutePath(jointPath))
  }
}