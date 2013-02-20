package uk.gov.tna.dri.schema

import scalaz._
import Scalaz._
import uk.gov.tna.dri.metadata.Row
import java.io.File
import util.parsing.input.Positional

abstract class Rule(val name: String, val argProvider: ArgProvider = Literal(None)) extends Positional {

  def evaluate(columnIndex: Int, row: Row, schema: Schema): ValidationNEL[String, Any] = {
    val cellValue = row.cells(columnIndex).value
    val ruleValue = argProvider.referenceValue(columnIndex, row, schema)
    if (pass(cellValue, ruleValue, schema.columnDefinitions(columnIndex))) true.successNel[String] else fail(ruleValue, columnIndex, row, schema)
  }

  def pass(cellValue: String, ruleValue: Option[String], columnDefinition: ColumnDefinition): Boolean

  def fail(columnIndex: Int, row: Row, schema: Schema): ValidationNEL[String, Any] = {
    fail(None, columnIndex, row, schema)
  }

  def fail(ruleValue: Option[String], columnIndex: Int, row: Row, schema: Schema): ValidationNEL[String, Any] = {
    val columnDefinition = schema.columnDefinitions(columnIndex)
    s"${toError} fails for line: ${row.lineNumber}, column: ${columnDefinition.id}, value: ${row.cells(columnIndex).value}".failNel[Any]
  }

  def toError = s"""${name}${argProvider.toError}"""
}

case class InRule(inValue: ArgProvider) extends Rule("in", inValue) {
  def pass(cellValue: String, ruleValue: Option[String], columnDefinition: ColumnDefinition): Boolean = {
    val (cellVal, ruleVal) = if (columnDefinition.directives.contains(IgnoreCase())) (cellValue.toLowerCase, ruleValue.get.toLowerCase) else (cellValue, ruleValue.get)
    ruleVal contains cellVal
  }
}

case class RegexRule(regex: ArgProvider) extends Rule("regex", regex) {
  def pass(cellValue: String, ruleValue: Option[String], columnDefinition: ColumnDefinition): Boolean = {
    val regex = if (columnDefinition.directives.contains(IgnoreCase())) "(?i)" + ruleValue.get else ruleValue.get
    cellValue matches regex
  }
}

case class FileExistsRule(rootPath: ArgProvider = Literal(None)) extends Rule("fileExists", rootPath) {
  def pass(cellValue: String, ruleValue: Option[String], columnDefinition: ColumnDefinition): Boolean = {
    val filePath = cellValue

    val fileExists = ruleValue match {
      case Some(rootPath) => new File(rootPath, filePath).exists()
      case None => new File(filePath).exists()
    }
    fileExists
  }
}

case class OrRule(left: Rule, right: Rule) extends Rule("or") {
  override def evaluate(columnIndex: Int, row: Row, schema: Schema): ValidationNEL[String, Any] = {
    left.evaluate(columnIndex, row, schema) match {
      case s@Success(_) => s
      case Failure(_) => right.evaluate(columnIndex, row, schema) match {
        case s@Success(_) => s
        case Failure(_) => fail(None, columnIndex, row, schema)
      }
    }
  }

  def pass(cellValue: String, ruleValue: Option[String], columnDefinition: ColumnDefinition): Boolean = true

  override def toError = s"""${left.toError} ${name} ${right.toError}"""
}