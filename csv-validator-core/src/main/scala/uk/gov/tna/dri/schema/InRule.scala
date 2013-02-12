package uk.gov.tna.dri.schema

import scalaz._
import Scalaz._
import uk.gov.tna.dri.metadata.Row
import util.Try

case class InRule(inVal: StringProvider) extends Rule {

  override def execute(columnIndex: Int, row: Row, schema: Schema): ValidationNEL[String, Any] = {
    val columnDefinition = schema.columnDefinitions(columnIndex)

    val columnDefWithValue = schema.columnDefinitions.zip(row.cells)
    val cellsByColumnId = columnDefWithValue.collect { case (x, y) => (x.id , y.value)} toMap

    val colVal = Try(inVal.getColumnValue(cellsByColumnId)).getOrElse("Invalid Column Name")

    if (schema.columnDefinitions(columnIndex).contains(IgnoreCase())) {
      if (colVal.toLowerCase.contains(row.cells(columnIndex).value.toLowerCase)) true.successNel[String]
      else s"inRule: ${colVal} fails for line ${row.lineNumber}, column: ${columnDefinition.id}, value: ${row.cells(columnIndex).value}".failNel[Any]
    } else {
      if (row.cells(columnIndex).value.contains(colVal)) true.successNel[String]
      if (colVal.contains(row.cells(columnIndex).value)) true.successNel[String]
      else s"inRule: ${colVal} fails for line ${row.lineNumber}, column: ${columnDefinition.id}, value: ${row.cells(columnIndex).value}".failNel[Any]
    }
  }
}




