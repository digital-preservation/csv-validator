package uk.gov.tna.dri.schema

import scalaz._
import Scalaz._
import uk.gov.tna.dri.metadata.Row
import util.matching.Regex
import java.io.File


abstract class Rule(argProvider: ArgProvider) {

  val name: String

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
    val rv = ruleValue.isEmpty.fold("", " " + ruleValue.get)
    s"${name}:${rv} fails for line: ${row.lineNumber}, column: ${columnDefinition.id}, value: ${row.cells(columnIndex).value}".failNel[Any]
  }
}

case class InRule(inVal: ArgProvider) extends Rule(inVal) {
  override val name = "in"

  def pass(cellValue: String, ruleValue: Option[String], columnDefinition: ColumnDefinition): Boolean = {
    val (cellVal, ruleVal) = if (columnDefinition.directives.contains(IgnoreCase())) (cellValue.toLowerCase, ruleValue.get.toLowerCase) else (cellValue, ruleValue.get)
    ruleVal contains cellVal
  }
}

case class RegexRule(regex: ArgProvider) extends Rule(regex) {

  val name = "regex"

  def pass(cellValue: String, ruleValue: Option[String], columnDefinition: ColumnDefinition): Boolean = {
    val regex = if (columnDefinition.directives.contains(IgnoreCase())) "(?i)" + ruleValue.get else ruleValue.get
    cellValue matches regex
  }
}

case class FileExistsRule(rootPath: ArgProvider) extends Rule(rootPath) {

  override val name = "fileExists"

  def pass(cellValue: String, ruleValue: Option[String], columnDefinition: ColumnDefinition): Boolean = {
    val filePath = cellValue

    val fileExists = ruleValue match {
      case Some(rootPath) => new File(rootPath, filePath).exists()
      case None => new File(filePath).exists()
    }

    fileExists
  }
}

