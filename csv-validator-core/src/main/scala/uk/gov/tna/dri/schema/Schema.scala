package uk.gov.tna.dri.schema

import util.Try

case class Schema(totalColumns: Int, columnDefinitions: List[ColumnDefinition]) {
  require(totalColumns == columnDefinitions.length, s"totalColumns: ${totalColumns} must be the same as the number of column definitions: ${columnDefinitions.size}")
}

case class ColumnDefinition(id: String, rules: List[Rule] = Nil, directives: List[ColumnDirective] = Nil) {
  def contains(columnDirective: ColumnDirective) = Try(directives.contains(columnDirective)).getOrElse(false)
}

trait ColumnDirective

case class Optional() extends ColumnDirective

case class IgnoreCase() extends ColumnDirective