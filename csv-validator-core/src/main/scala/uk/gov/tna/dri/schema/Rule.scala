package uk.gov.tna.dri.schema

import scalaz._
import Scalaz._
import uk.gov.tna.dri.metadata.Row
import java.io.File
import util.parsing.input.Positional

abstract class Rule(val name: String, val argProvider: ArgProvider = Literal(None)) extends Positional {

  val Uuid4Regex = "[a-f0-9]{8}-[a-f0-9]{4}-4[a-f0-9]{3}-[89ab][a-f0-9]{3}-[a-f0-9]{12}"

  def evaluate(columnIndex: Int, row: Row, schema: Schema): ValidationNEL[String, Any] = {
    val cellValue = row.cells(columnIndex).value
    val ruleValue = argProvider.referenceValue(columnIndex, row, schema)
    if (valid(cellValue, ruleValue, schema.columnDefinitions(columnIndex))) true.successNel[String] else fail(ruleValue, columnIndex, row, schema)
  }

  def valid(cellValue: String, ruleValue: Option[String], columnDefinition: ColumnDefinition): Boolean

  def fail(columnIndex: Int, row: Row, schema: Schema): ValidationNEL[String, Any] = {
    fail(None, columnIndex, row, schema)
  }

  def fail(ruleValue: Option[String], columnIndex: Int, row: Row, schema: Schema): ValidationNEL[String, Any] = {
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
        case Failure(_) => fail(None, columnIndex, row, schema)
      }
    }
  }

  def valid(cellValue: String, ruleValue: Option[String], columnDefinition: ColumnDefinition): Boolean = true

  override def toError = s"""${left.toError} ${name} ${right.toError}"""
}

case class RegexRule(regex: ArgProvider) extends Rule("regex", regex) {
  def valid(cellValue: String, ruleValue: Option[String], columnDefinition: ColumnDefinition): Boolean = {
    val regex = if (columnDefinition.directives.contains(IgnoreCase())) "(?i)" + ruleValue.get else ruleValue.get
    cellValue matches regex
  }
}

case class FileExistsRule(rootPath: ArgProvider = Literal(None)) extends Rule("fileExists", rootPath) {
  def valid(cellValue: String, ruleValue: Option[String], columnDefinition: ColumnDefinition): Boolean = {
    val filePath = cellValue

    val fileExists = ruleValue match {
      case Some(rootPath) => new File(rootPath, filePath).exists()
      case None => new File(filePath).exists()
    }
    fileExists
  }
}

trait RuleHelper {
  def valid(cellValue: String, ruleValue: Option[String], columnDefinition: ColumnDefinition, matcher: (String, String) => Boolean) = {
    val (cv, rv) = if (columnDefinition.directives.contains(IgnoreCase())) (cellValue.toLowerCase, ruleValue.get.toLowerCase) else (cellValue, ruleValue.get)
    matcher(rv, cv)
  }
}

case class InRule(inValue: ArgProvider) extends Rule("in", inValue) with RuleHelper {
  def valid(cellValue: String, ruleValue: Option[String], columnDefinition: ColumnDefinition) = valid(cellValue, ruleValue, columnDefinition, _ contains _)
}

case class IsRule(isValue: ArgProvider) extends Rule("is", isValue) with RuleHelper {
  def valid(cellValue: String, ruleValue: Option[String], columnDefinition: ColumnDefinition) = valid(cellValue, ruleValue, columnDefinition, _ ==  _)
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
  val positiveIntegerRegex = "[1-9][0-9]+"
  def valid(cellValue: String, ruleValue: Option[String], columnDefinition: ColumnDefinition) = cellValue matches positiveIntegerRegex
}