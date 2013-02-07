package uk.gov.tna.dri.metadata

case class MetaData (rows: List[Row])

object MetaData {
  def fromStrings(rows: List[List[String]]) = new MetaData(rows.zipWithIndex.map{ case (r,i) => Row.fromStrings(r, i + 1) })
}

case class Row (cells: List[Cell], lineNumber: Int)

object Row {
  def fromStrings(row: List[String], lineNumber: Int) = new Row(row map (Cell(_)), lineNumber)
}

case class Cell(value: String)
