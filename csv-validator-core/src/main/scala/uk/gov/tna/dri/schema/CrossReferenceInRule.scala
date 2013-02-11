package uk.gov.tna.dri.schema

import uk.gov.tna.dri.metadata.Row
import scalaz._
import Scalaz._

case class SchemaX(t: Int, columnDefinitions: List[ColumnDefinitionX])

case class ColumnDefinitionX(id: String, rules: List[RuleX] = Nil)

trait RuleX {
  def execute(columnIndex: Int, row: Row, schema: SchemaX): ValidationNEL[String, Any]
}

case class CrossReferenceInRule(crossReferenceId: String) extends RuleX {

  override def execute(columnIndex: Int, row: Row, schema: SchemaX): ValidationNEL[String, Any] = {
    if (!schema.columnDefinitions.exists(_.id == crossReferenceId)) {
      ("in($" + s"${crossReferenceId}) references a non-existent column").failNel[Any]
    } else {
      val referencedIndex = schema.columnDefinitions.indexWhere(_.id == crossReferenceId)
      val columnDefinition = schema.columnDefinitions(columnIndex)

      if (row.cells(referencedIndex).value contains row.cells(columnIndex).value) row.successNel[String]
        else ("in($" + s"${crossReferenceId}) fails for line ${row.lineNumber}, column: ${columnDefinition.id}, value: ${row.cells(columnIndex).value}").failNel[Any]
    }
  }
}

