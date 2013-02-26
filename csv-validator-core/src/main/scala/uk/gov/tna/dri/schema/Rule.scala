package uk.gov.tna.dri.schema

import scalaz._
import Scalaz._
import uk.gov.tna.dri.metadata.Row
import java.io.{BufferedInputStream, FileInputStream, File}
import util.parsing.input.Positional
import collection.mutable
import java.security.MessageDigest

abstract class Rule(val name: String, val argProvider: ArgProvider = Literal(None)) extends Positional {

  val Uuid4Regex = "[a-f0-9]{8}-[a-f0-9]{4}-4[a-f0-9]{3}-[89ab][a-f0-9]{3}-[a-f0-9]{12}"

  def evaluate(columnIndex: Int, row: Row, schema: Schema): ValidationNEL[String, Any] = {
    val cellValue = row.cells(columnIndex).value
    val ruleValue = argProvider.referenceValue(columnIndex, row, schema)
    if (valid(cellValue, ruleValue, schema.columnDefinitions(columnIndex))) true.successNel[String] else fail(columnIndex, row, schema)
  }

  def valid(cellValue: String, ruleValue: Option[String], columnDefinition: ColumnDefinition): Boolean

  def fail(columnIndex: Int, row: Row, schema: Schema): ValidationNEL[String, Any] = {
    val columnDefinition = schema.columnDefinitions(columnIndex)
    s"${toError} fails for line: ${row.lineNumber}, column: ${columnDefinition.id}, value: ${row.cells(columnIndex).value}".failNel[Any]
  }

  def toError = s"""${name}${argProvider.toError}"""
}

case class OrRule(left: Rule, right: Rule) extends Rule("or") {
  override def evaluate(columnIndex: Int, row: Row, schema: Schema): ValidationNEL[String, Any] = {
    left.evaluate(columnIndex, row, schema) match {
      case s @ Success(_) => s
      case Failure(_) => right.evaluate(columnIndex, row, schema) match {
        case s @ Success(_) => s
        case Failure(_) => fail(columnIndex, row, schema)
      }
    }
  }

  def valid(cellValue: String, ruleValue: Option[String], columnDefinition: ColumnDefinition) = true

  override def toError = s"""${left.toError} ${name} ${right.toError}"""
}

case class RegexRule(regex: ArgProvider) extends Rule("regex", regex) {
  def valid(cellValue: String, ruleValue: Option[String], columnDefinition: ColumnDefinition) = {
    val regex = if (columnDefinition.directives.contains(IgnoreCase())) "(?i)" + ruleValue.get else ruleValue.get
    cellValue matches regex
  }
}

case class FileExistsRule(rootPath: ArgProvider = Literal(None)) extends Rule("fileExists", rootPath) {
  def valid(cellValue: String, ruleValue: Option[String], columnDefinition: ColumnDefinition) = {
    val filePath = cellValue

    val fileExists = ruleValue match {
      case Some(rootPath) => new File(rootPath, filePath).exists()
      case None => new File(filePath).exists()
    }

    fileExists
  }
}

case class InRule(inValue: ArgProvider) extends Rule("in", inValue) {
  def valid(cellValue: String, ruleValue: Option[String], columnDefinition: ColumnDefinition): Boolean = {
    val (rv, cv) = if (columnDefinition.directives.contains(IgnoreCase())) (ruleValue.get.toLowerCase, cellValue.toLowerCase) else (ruleValue.get, cellValue)
    rv contains cv
  }
}

case class IsRule(isValue: ArgProvider) extends Rule("is", isValue) {
  def valid(cellValue: String, ruleValue: Option[String], columnDefinition: ColumnDefinition) = {
    val (rv, cv) = if (columnDefinition.directives.contains(IgnoreCase())) (ruleValue.get.toLowerCase, cellValue.toLowerCase) else (ruleValue.get, cellValue)
    cv == rv
  }
}

case class IsNotRule(isValue: ArgProvider) extends Rule("isNot", isValue) {
  def valid(cellValue: String, ruleValue: Option[String], columnDefinition: ColumnDefinition) = {
    val (rv, cv) = if (columnDefinition.directives.contains(IgnoreCase())) (ruleValue.get.toLowerCase, cellValue.toLowerCase) else (ruleValue.get, cellValue)
    cv != rv
  }
}

case class StartsRule(isValue: ArgProvider) extends Rule("starts", isValue) {
  def valid(cellValue: String, ruleValue: Option[String], columnDefinition: ColumnDefinition) = {
    val (rv, cv) = if (columnDefinition.directives.contains(IgnoreCase())) (ruleValue.get.toLowerCase, cellValue.toLowerCase) else (ruleValue.get, cellValue)
    cv startsWith rv
  }
}

case class EndsRule(isValue: ArgProvider) extends Rule("ends", isValue) {
  def valid(cellValue: String, ruleValue: Option[String], columnDefinition: ColumnDefinition) = {
    val (rv, cv) = if (columnDefinition.directives.contains(IgnoreCase())) (ruleValue.get.toLowerCase, cellValue.toLowerCase) else (ruleValue.get, cellValue)
    cv endsWith rv
  }
}

case class UriRule() extends Rule("uri") {
  val uriRegex = "http://datagov.nationalarchives.gov.uk/66/WO/409/[0-9]+/[0-9]+/" + Uuid4Regex
  def valid(cellValue: String, ruleValue: Option[String], columnDefinition: ColumnDefinition) = cellValue matches uriRegex
}

case class XsdDateTimeRule() extends Rule("xDateTime") {
  val xsdDateTimeRegex = "[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}"
  def valid(cellValue: String, ruleValue: Option[String], columnDefinition: ColumnDefinition) = cellValue matches xsdDateTimeRegex
}

case class XsdDateRule() extends Rule("xDate") {
  val xsdDateRegex = "[0-9]{4}-[0-9]{2}-[0-9]{2}"
  def valid(cellValue: String, ruleValue: Option[String], columnDefinition: ColumnDefinition) = cellValue matches xsdDateRegex
}

case class UkDateRule() extends Rule("ukDate") {
  val ukDateRegex = "[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}"
  def valid(cellValue: String, ruleValue: Option[String], columnDefinition: ColumnDefinition) = cellValue matches ukDateRegex
}

case class XsdTimeRule() extends Rule("xTime") {
  val xsdTimeRegex = "[0-9]{2}:[0-9]{2}:[0-9]{2}"
  def valid(cellValue: String, ruleValue: Option[String], columnDefinition: ColumnDefinition) = cellValue matches xsdTimeRegex
}

case class Uuid4Rule() extends Rule("uuid4") {
  def valid(cellValue: String, ruleValue: Option[String], columnDefinition: ColumnDefinition) = cellValue matches Uuid4Regex
}

case class PositiveIntegerRule() extends Rule("positiveInteger") {
  val positiveIntegerRegex = "[0-9]+"
  def valid(cellValue: String, ruleValue: Option[String], columnDefinition: ColumnDefinition) = cellValue matches positiveIntegerRegex
}

case class UniqueRule() extends Rule("unique") {
  val distinctValues = mutable.HashMap[String, Int]()

  override def evaluate(columnIndex: Int, row: Row, schema: Schema): ValidationNEL[String, Any] = {
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
        s"${toError} fails for line: ${row.lineNumber}, column: ${columnDefinition.id}, value: ${row.cells(columnIndex).value} (original at line: ${distinctValues(o)})".failNel[Any]
      }
    }
  }

  def valid(cellValue: String, ruleValue: Option[String], columnDefinition: ColumnDefinition) = true
}

case class ChecksumRule(rootPath: ArgProvider, file: ArgProvider, algorithm: String) extends Rule("checksum") {
  def this(file: ArgProvider, algorithm: String) = this(Literal(None), file, algorithm)

  override def evaluate(columnIndex: Int, row: Row, schema: Schema): ValidationNEL[String, Any] = {
    val cellValue = row.cells(columnIndex).value
    val columnDefinition = schema.columnDefinitions(columnIndex)

    if (checksum(filename(columnIndex, row, schema)) == cellValue) true.successNel
    else s"${toError} fails for line: ${row.lineNumber}, column: ${columnDefinition.id}, value: ${row.cells(columnIndex).value}".failNel[Any]
  }

  def filename(columnIndex: Int, row: Row, schema: Schema): String = {
    val f = file.referenceValue(columnIndex, row, schema).get

    rootPath.referenceValue(columnIndex, row, schema) match {
      case None => f
      case Some(r: String) if r.endsWith("/") => r + f
      case Some(r) => r + "/" + f
    }
  }

  override def toError = {
    val rootPathError = if (rootPath.toError.isEmpty) "" else rootPath.toError + ", "
    s"""${name}(file${rootPathError}${file.toError})"""
  }

  def valid(cellValue: String, ruleValue: Option[String], columnDefinition: ColumnDefinition) = true

  private def checksum(filename: String ): String = {
    val digest = MessageDigest.getInstance(algorithm)
    val fileBuffer = new BufferedInputStream(new FileInputStream(filename))
    Stream.continually(fileBuffer.read).takeWhile(-1 !=).map(_.toByte).foreach( digest.update(_))
    hexEncode(digest.digest)
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
    sb.toString
  }
}