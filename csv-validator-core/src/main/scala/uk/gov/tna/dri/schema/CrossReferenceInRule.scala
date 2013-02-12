package uk.gov.tna.dri.schema

import uk.gov.tna.dri.metadata.Row
import scalaz._
import Scalaz._

case class CrossReferenceInRule(crossReferenceId: String) extends Rule {

  override def execute(columnIndex: Int, row: Row, schema: Schema): ValidationNEL[String, Any] = {
    if (!schema.columnDefinitions.exists(_.id == crossReferenceId)) {
      ("in($" + s"${crossReferenceId}) references a non-existent column").failNel[Any]
    } else {
      val referencedIndex = schema.columnDefinitions.indexWhere(_.id == crossReferenceId)
      val columnDefinition = schema.columnDefinitions(columnIndex)


      if (columnDefinition.contains(IgnoreCase())) {
        if (row.cells(referencedIndex).value.toLowerCase() contains row.cells(columnIndex).value.toLowerCase()) row.successNel[String]
        else ("in($" + s"${crossReferenceId}) fails for line ${row.lineNumber}, column: ${columnDefinition.id}, value: ${row.cells(columnIndex).value}").failNel[Any]
      } else {
        if (row.cells(referencedIndex).value contains row.cells(columnIndex).value) row.successNel[String]
        else ("in($" + s"${crossReferenceId}) fails for line ${row.lineNumber}, column: ${columnDefinition.id}, value: ${row.cells(columnIndex).value}").failNel[Any]
      }
    }
  }
}