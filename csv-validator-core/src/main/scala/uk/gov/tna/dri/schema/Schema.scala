package uk.gov.tna.dri.schema

import uk.gov.tna.dri.metadata.Row
import util.parsing.input.Positional

case class Schema(globalDirectives: List[GlobalDirective], columnDefinitions: List[ColumnDefinition])

trait GlobalDirective extends Positional

case class Separator(separatorChar: Char) extends GlobalDirective

case class Quoted() extends GlobalDirective

case class TotalColumns(numberOfColumns: Int) extends GlobalDirective

case class NoHeader() extends GlobalDirective

case class IgnoreColumnNameCase() extends GlobalDirective

case class ColumnDefinition(id: String, rules: List[Rule] = Nil, directives: List[ColumnDirective] = Nil)

trait ArgProvider {

  def referenceValue(columnIndex: Int, row: Row, schema: Schema): Option[String]

  def toError:String
}

case class ColumnReference(value: String) extends ArgProvider {

  def referenceValue(columnIndex: Int, row: Row, schema: Schema): Option[String] = {
    val referencedIndex = schema.columnDefinitions.indexWhere(_.id == value)
    Some(row.cells(referencedIndex).value)
  }

  def toError ="($" + value +")"
}

case class Literal(value: Option[String]) extends ArgProvider {

  def referenceValue(columnIndex: Int, row: Row, schema: Schema): Option[String] = value

  def toError = if (value.isDefined) "(\"" + value.get +"\")" else ""
}

trait ColumnDirective extends Positional

case class Optional() extends ColumnDirective {
  override def toString(): String = "Optional"
}

case class IgnoreCase() extends ColumnDirective  {
  override def toString = "IgnoreCase"
}