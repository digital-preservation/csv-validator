package uk.gov.tna.dri.schema

import scalaz.{Success => SuccessZ, Failure => FailureZ}

import scalax.file.{Path, PathSet}
import scalaz.Scalaz._
import scala.Some
import uk.gov.tna.dri.metadata.Row
import java.security.MessageDigest
import java.io.{File, FileInputStream, BufferedInputStream}
import scala.util.Try
import scala.annotation.tailrec
import java.net.URI

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

case class UriRule() extends PatternRule("uri", UriRegex)

case class Uuid4Rule() extends PatternRule("uuid4", Uuid4Regex)

case class PositiveIntegerRule() extends PatternRule("positiveInteger", PositiveIntegerRegex)



case class ChecksumRule(rootPath: ArgProvider, file: ArgProvider, algorithm: String, pathSubstitutions: List[(String,String)]) extends Rule("checksum", rootPath, file) with FileWildcardSearch[String] {
  def this(file: ArgProvider, algorithm: String, pathSubstitutions: List[(String,String)]) = this(Literal(None), file, algorithm, pathSubstitutions)
  def this(file: ArgProvider, algorithm: String) = this(Literal(None), file, algorithm, List[(String,String)]())

  override def evaluate(columnIndex: Int, row: Row, schema: Schema): RuleValidation[Any] = {
    val columnDefinition = schema.columnDefinitions(columnIndex)

    search(filename(columnIndex, row, schema)) match {
      case SuccessZ(hexValue: String) if hexValue == cellValue(columnIndex,row,schema) => true.successNel[String]
      case SuccessZ(hexValue: String) => s"$toError file ${'"'}${filename(columnIndex, row, schema)._1}${filename(columnIndex, row, schema)._2}${'"'} checksum match fails for line: ${row.lineNumber}, column: ${columnDefinition.id}, ${toValueError(row,columnIndex)}".failNel[Any]
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

  def matchWildcardPaths(matchList: PathSet[Path],fullPath: String): ValidationNEL[String, String] = matchList.size match {
    case 1 => calcChecksum(matchList.head.path)
    case 0 => s"""no files for $fullPath found""".failNel[String]
    case _ => s"""multiple files for $fullPath found""".failNel[String]
  }

  def matchSimplePath(fullPath: String): ValidationNEL[String, String]  = calcChecksum(fullPath)

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
      if (p.contains("*")) findBaseRecur(Path.fromString(p).parent.get.path, Path.fromString(p).name + "/" + f)
      else (p, f)
    }

    if (path.startsWith("file://"))  {
      val pathURI = Path(new URI( FileSystem.replaceSpaces(path))).get
      findBaseRecur("file://" + pathURI.parent.get.path, pathURI.name)
    } else if (Path.fromString(path).parent.isEmpty) ("./", path) else findBaseRecur(Path.fromString(path).parent.get.path, Path.fromString(path).name)
  }

  def search(filePaths: (String, String)): ValidationNEL[String, T] = {
    try {
      val fullPath = new FileSystem( None, filePaths._1 + filePaths._2, pathSubstitutions).expandBasePath
      val (basePath,matchPath ) = findBase(fullPath)

      val path: Option[Path] = {
        FileSystem.createFile( basePath ) match {
          case scala.util.Success(f) => Some(Path(f))
          case scala.util.Failure(_) => None
        }
      }

      def pathString = s"${filePaths._1} (localfile: $fullPath)"

      def findMatches(wc: (Path, String) => PathSet[Path] ): ValidationNEL[String, T] = {
        path match {
          case Some(p) =>  matchWildcardPaths( wc(p, matchPath ), fullPath )
          case None => "no file".failNel[T]
        }
      }

      def basePathExists: Boolean =   filePaths._1.length>0 && (!(FileSystem.createFile( basePath ) match {
        case scala.util.Success(f) =>   f.exists
        case scala.util.Failure(_) => false
      }))

      def wildcardNotInRoot: Boolean = filePaths._1.contains("*")

      def matchUsesWildDirectory: Boolean = matchPath.contains("**")

      def matchUsesWildFiles: Boolean = matchPath.contains("*")

      def fileExists: Boolean = {
        val path = basePath+System.getProperty("file.separator") + matchPath

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
  def createFile( filename:String): Try[File] =  Try{ if( filename.startsWith("file:")) new File( new URI(filename)) else  new File( filename )}

  def replaceSpaces( file: String): String = file.replace(" ", "%20")

  private def file2PlatformDependent( file: String): String =
    if ( System.getProperty("file.separator") == "/" ) file.replace('\\', '/')
    else file.replace('/', '\\')

  def convertPath2Platform(filename: String): String = {
    if ( filename.startsWith("file://"))  replaceSpaces(filename) else file2PlatformDependent( filename )
  }
}

case class FileSystem(basePath: Option[String], file: String, pathSubstitutions: List[(String,String)] ) {

  def this( root:String, file: String, pathSubstitutions: List[(String,String)] ) = this( Some(root), file, pathSubstitutions)

  def this( file: String, pathSubstitutions: List[(String,String)]) = this(None, file, pathSubstitutions)

  val separator: Char = System.getProperty("file.separator").head

  private def substitutePath(  filename: String): String = {
    val x = pathSubstitutions.filter{ case (subFrom, _) => filename.contains(subFrom)}.map{ case (subFrom, subTo) => filename.replaceFirst(subFrom, subTo) }
    if (x.isEmpty) filename else x.head
  }

  def jointPath: String = {
    val fs: Char = System.getProperty("file.separator").head

    basePath match {
      case Some(bp) =>
        if (bp.length > 0 && bp.last != fs && file.head != fs) bp + fs + file
        else if (bp.length > 0 && bp.last == fs && file.head == fs) bp + file.tail
        else bp + file

      case None => file
    }
  }

  def exists: Boolean = {
    FileSystem.createFile( FileSystem.convertPath2Platform( substitutePath(jointPath))) match {
      case scala.util.Success(f) => f.exists
      case scala.util.Failure(_) => false
    }
  }

  def expandBasePath: String = {
    if ( basePath.isEmpty || basePath.getOrElse("") == "")  FileSystem.file2PlatformDependent(substitutePath(file))
    else FileSystem.file2PlatformDependent(substitutePath(jointPath))
  }
}

