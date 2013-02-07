package uk.gov.tna.dri.schema

import util.Try

case class Schema(totalColumns: Int, columnDefinitions: List[ColumnDefinition]) {
  require(totalColumns == columnDefinitions.length, s"totalColumns: ${totalColumns} must be the same as the number of column definitions: ${columnDefinitions.size}")
}

case class ColumnDefinition(id: String, rules: List[Rule] = Nil, directives: List[ColumnDirective] = Nil) {
  def ignoreCase = Try(directives.contains(IgnoreCase())).getOrElse(false)
}

abstract class StringProvider(val value: String) {
  def getColumnValue(colToValMap: Map[String, String]): String
}

case class ColumnTypeProvider(override val value: String) extends StringProvider(value) {
  def getColumnValue(colToValMap: Map[String, String]): String = colToValMap(value.tail)
}

case class LiteralTypeProvider(override val value: String) extends StringProvider(value) {
  def getColumnValue(colToValMap: Map[String, String]): String = value
}

trait ColumnDirective

case class Optional() extends ColumnDirective

case class IgnoreCase() extends ColumnDirective