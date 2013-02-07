package uk.gov.tna.dri.validator

import uk.gov.tna.dri.schema.{ColumnDefinition, Schema}
import au.com.bytecode.opencsv.CSVReader
import java.io.Reader
import scala.collection.JavaConversions._
import scalaz._
import Scalaz._

trait MetaDataValidator {

  type MetaDataValidation[A] = ValidationNEL[String, A]

  def validate(csv: Reader, schema: Schema) = {
    val rows = new CSVReader(csv).readAll().toList
    validateMetaData(MetaData.fromStrings(rows.map(_.toList)), schema)
  }

  private def validateMetaData(metaData: MetaData, schema: Schema) = {
    val v = for (row <- metaData.rows) yield validateRow(row, schema)
    v.sequence[MetaDataValidation, Any]
  }

  private def validateRow(row: Row, schema: Schema) = {
    val totalColumnsV = totalColumns(row, schema)
    val rulesV = rules(row, schema)
    (totalColumnsV |@| rulesV) { _ :: _ }
  }

  private def totalColumns(row: Row, schema: Schema) = {
    if (row.cells.length == schema.totalColumns) true.successNel[String] else s"Expected @TotalColumns of ${schema.totalColumns} and found ${row.cells.length} on line ${row.lineNumber}".failNel[Any]
  }

  private def rules(row: Row, schema: Schema) = {
    val columnDefWithValue = schema.columnDefinitions.zip(row.cells)
    val myMap = columnDefWithValue.collect{
      case (colDef, cell) => (colDef.id , cell.value)
    } toMap

    val cells = row.cells.lift
    val v = for { (columnDefinition, columnIndex) <- schema.columnDefinitions.zipWithIndex } yield validateCell(myMap, row.lineNumber, cells(columnIndex), columnDefinition)

    v.sequence[MetaDataValidation, Any]
  }

  def validateCell(colToMap: Map[String, String], lineNumber: Int, cell: Option[Cell], columnDefinition: ColumnDefinition) = cell match {
    case Some(c) => rulesForCell(colToMap, lineNumber,c, columnDefinition)
    case _ => s"Missing value at line: ${lineNumber}, column: ${columnDefinition.id}".failNel[Any]
  }

  private def rulesForCell(colToMap: Map[String, String], lineNumber: Int, cell: Cell, columnDefinition: ColumnDefinition) = {
    columnDefinition.rules.map(_.execute(lineNumber, colToMap, columnDefinition, cell.value)).sequence[MetaDataValidation, Any]
  }

  case class MetaData (rows: List[Row])

  object MetaData {
    def fromStrings(rows: List[List[String]]) = new MetaData(rows.zipWithIndex.map{ case (r,i) => Row.fromStrings(r, i + 1) })
  }

  case class Row (cells: List[Cell], lineNumber: Int)

  object Row {
    def fromStrings(row: List[String], lineNumber: Int) = new Row(row map (Cell(_)), lineNumber)
  }

  case class Cell(value: String)
}
