package uk.gov.tna.dri.schema

case class Schema(totalColumns: Int, columns: List[ColumnDefinition]) {
  require(totalColumns == columns.size, s"totalColumns: ${totalColumns} must be the same as the number of column definitions: ${columns.size}")
}