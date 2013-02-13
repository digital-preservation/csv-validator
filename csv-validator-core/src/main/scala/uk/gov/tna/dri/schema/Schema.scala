package uk.gov.tna.dri.schema

import util.Try
import uk.gov.tna.dri.metadata.Row

case class Schema(totalColumns: Int, columnDefinitions: List[ColumnDefinition]) {
  require(totalColumns == columnDefinitions.length, s"totalColumns: ${totalColumns} must be the same as the number of column definitions: ${columnDefinitions.size}")
}

case class ColumnDefinition(id: String, rules: List[Rule] = Nil, directives: List[ColumnDirective] = Nil) {
  def contains(columnDirective: ColumnDirective) = Try(directives.contains(columnDirective)).getOrElse(false)
}

abstract class StringProvider(val value: String) {
  def referenceValue(columnIndex: Int, row: Row, schema: Schema): Either[String, String]
}

case class ColumnTypeProvider(override val value: String) extends StringProvider(value) {
  def referenceValue(columnIndex: Int, row: Row, schema: Schema): Either[String, String] = {
    if (!schema.columnDefinitions.exists(_.id == value)) {
      Left("in($" + s"${value}) references a non-existent column")
    } else {
      val referencedIndex = schema.columnDefinitions.indexWhere(_.id == value)
      Right(row.cells(referencedIndex).value)
    }
  }
}

case class LiteralTypeProvider(override val value: String) extends StringProvider(value) {
  def referenceValue(columnIndex: Int, row: Row, schema: Schema): Either[String, String] = Right(value)
}

trait ColumnDirective

case class Optional() extends ColumnDirective

case class IgnoreCase() extends ColumnDirective