package uk.gov.tna.dri.schema

import uk.gov.tna.dri.metadata.Row
import scalaz._
import Scalaz._

case class SchemaX(columnDefinitions: List[ColumnDefinitionX])

case class ColumnDefinitionX(id: String, rules: List[RuleX] = Nil)

trait RuleX {
  def execute(columnIndex: Int, row: Row, schema: SchemaX): ValidationNEL[String, Any]
}

case class CrossReferenceInRule(crossReference: ColumnDefinitionX) extends RuleX {

  override def execute(columnNumber: Int, row: Row, schema: SchemaX): ValidationNEL[String, Any] = {
    val referencedIndex = schema.columnDefinitions.indexOf(crossReference)

    if (row.cells(referencedIndex).value contains row.cells(toIndex(columnNumber)).value) row.successNel[String]
    else "fail".failNel[Row]
  }

  private def toIndex(columnNumber: Int) = columnNumber - 1
}

