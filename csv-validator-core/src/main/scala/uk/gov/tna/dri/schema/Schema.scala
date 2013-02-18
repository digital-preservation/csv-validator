package uk.gov.tna.dri.schema

import uk.gov.tna.dri.metadata.Row

case class Schema(totalColumns: Int, columnDefinitions: List[ColumnDefinition])

case class ColumnDefinition(id: String, rules: List[Rule] = Nil, directives: List[ColumnDirective] = Nil)

trait ArgProvider {
  def argValue: Option[String]

  def referenceValue(columnIndex: Int, row: Row, schema: Schema): Option[String]
}

case class ColumnReference(value: String) extends ArgProvider {
  def argValue: Option[String] = Some(value)

  def referenceValue(columnIndex: Int, row: Row, schema: Schema): Option[String] = {
    val referencedIndex = schema.columnDefinitions.indexWhere(_.id == value)
    Some(row.cells(referencedIndex).value)
  }
}

case class Literal(value: Option[String]) extends ArgProvider {
  def argValue: Option[String] = value

  def referenceValue(columnIndex: Int, row: Row, schema: Schema): Option[String] = value
}

trait ColumnDirective

case class Optional() extends ColumnDirective

case class IgnoreCase() extends ColumnDirective