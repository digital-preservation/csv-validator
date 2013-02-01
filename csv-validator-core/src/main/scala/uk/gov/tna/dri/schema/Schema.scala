package uk.gov.tna.dri.schema

case class Schema(totalColumns: Int, columnDefinitions: List[ColumnDefinition]) {
  require(totalColumns == columnDefinitions.length, s"totalColumns: ${totalColumns} must be the same as the number of column definitions: ${columnDefinitions.size}")
}