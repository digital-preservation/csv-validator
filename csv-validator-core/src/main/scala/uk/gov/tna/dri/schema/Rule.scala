package uk.gov.tna.dri.schema

import util.matching.Regex
import scalaz._
import Scalaz._
import uk.gov.tna.dri.metadata.Row

case class CellContext(columnIndex: Int, row: Row, schema: Schema) {
  lazy val cell = row.cells(columnIndex)
  lazy val columnIdentifier = schema.columnDefinitions(columnIndex).id
  lazy val lineNumber = row.lineNumber
  lazy val rules = schema.columnDefinitions(columnIndex).rules
  lazy val columnDefWithValue = schema.columnDefinitions.zip(row.cells)
  lazy val cellsByColumnId = columnDefWithValue.collect { case (x, y) => (x.id , y.value)} toMap
  lazy val columnDirectives = schema.columnDefinitions(columnIndex).directives
}

sealed trait Rule {
  def execute(cellContext: CellContext): ValidationNEL[String, Any]
}

case class RegexRule(regex: Regex) extends Rule {
  override def execute(cellContext: CellContext): ValidationNEL[String, Any] = {
    val exp = regex.pattern.pattern

    if (cellContext.cell.value matches exp) true.successNel[String]
    else s"regex: ${exp} fails for line ${cellContext.lineNumber}, column: ${cellContext.columnIdentifier}".failNel[Any]
  }
}

case class InRule(inVal: StringProvider) extends Rule {
  override def execute(cellContext: CellContext): ValidationNEL[String, Any] = {
    val colVal = util.Try (inVal.getColumnValue(cellContext.cellsByColumnId)).getOrElse("Invalid Column Name")

    if (cellContext.cell.value.contains(colVal)) true.successNel[String]
    else s"inRule: ${colVal} fails for line ${cellContext.lineNumber}, column: ${cellContext.columnIdentifier}, value: ${cellContext.cell.value}".failNel[Any]
  }
}
