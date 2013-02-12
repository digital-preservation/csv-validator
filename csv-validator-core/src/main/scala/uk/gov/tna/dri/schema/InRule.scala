package uk.gov.tna.dri.schema

import scalaz._
import Scalaz._
import uk.gov.tna.dri.metadata.Row

case class InRule(inVal: StringProvider) extends Rule {

  override def execute(columnIndex: Int, row: Row, schema: Schema): ValidationNEL[String, Any] = {
    val columnDefinition = schema.columnDefinitions(columnIndex)

    inVal.cellValue(columnIndex, row, schema) match {
      case Left(e) => e.failNel[Any]

      case Right(ruleValue) => {
        val cellValue = row.cells(columnIndex).value

        if (schema.columnDefinitions(columnIndex).contains(IgnoreCase())) {
          if (ruleValue.toLowerCase.contains(cellValue.toLowerCase))
            true.successNel[String]
          else
            s"in: ${ruleValue} fails for line ${row.lineNumber}, column: ${columnDefinition.id}, value: ${cellValue}".failNel[Any]
        } else {
          if (ruleValue.contains(cellValue))
            true.successNel[String]
          else
            s"in: ${ruleValue} fails for line ${row.lineNumber}, column: ${columnDefinition.id}, value: ${cellValue}".failNel[Any]
        }
      }
    }
  }
}