package uk.gov.tna.dri.schema

import scalaz._
import Scalaz._
import uk.gov.tna.dri.metadata.Row

case class InRule(value: String) extends Rule {

  override def execute(columnIndex: Int, row: Row, schema: Schema): ValidationNEL[String, Any] = {
    val columnDefinition = schema.columnDefinitions(columnIndex)

    if (schema.columnDefinitions(columnIndex).contains(IgnoreCase())) {
      if (value.toLowerCase.contains(row.cells(columnIndex).value.toLowerCase))
        true.successNel[String]
      else
        s"inRule: ${value} fails for line ${row.lineNumber}, column: ${columnDefinition.id}, value: ${row.cells(columnIndex).value}".failNel[Any]
    } else {
      if (value.contains(row.cells(columnIndex).value))
        true.successNel[String]
      else
        s"inRule: ${value} fails for line ${row.lineNumber}, column: ${columnDefinition.id}, value: ${row.cells(columnIndex).value}".failNel[Any]
    }
  }
}