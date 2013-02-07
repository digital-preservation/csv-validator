package uk.gov.tna.dri.metadata

case class Row(cells: List[Cell], lineNumber: Int)

case class Cell(value: String)
