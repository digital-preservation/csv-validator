package uk.gov.tna.dri.validator

import uk.gov.tna.dri.schema.{ColumnDefinition, Schema}
import au.com.bytecode.opencsv.CSVReader
import java.io.Reader
import scala.collection.JavaConversions._
import scalaz._
import Scalaz._

trait MetaDataValidator {

  def validate(csv: Reader, schema: Schema) = {
    val rows = new CSVReader(csv).readAll()

    val v = for ((row, rowIndex) <- rows.zipWithIndex) yield validateRow(rowIndex + 1, row.toList, schema)
    v.sequence[({type x[a] = ValidationNEL[String, a]})#x, List[Any]]
  }

  private def validateRow(lineNumber: Int, row: List[String], schema: Schema) = {
    val totalColumnsV = totalColumns(lineNumber, row, schema)
    val rulesV = rules(lineNumber, row, schema)
    (totalColumnsV |@| rulesV) { _ :: _ }
  }

  private def totalColumns(lineNumber: Int, row: List[String], schema: Schema) = {
    if (row.length == schema.totalColumns) true.successNel[String] else s"Expected @TotalColumns of ${schema.totalColumns} and found ${row.length} on line ${lineNumber}".failNel[Any]
  }

  private def rules(lineNumber: Int, row: List[String], schema: Schema) = {
    val cells = row.lift
    val v = for { (columnDefinition, columnIndex) <- schema.columnDefinitions.zipWithIndex } yield validateCell(lineNumber, cells(columnIndex), columnDefinition)

    v.sequence[({type x[a] = ValidationNEL[String, a]})#x, Any]
  }

  def validateCell(lineNumber: Int, cell: Option[String], columnDefinition: ColumnDefinition) = cell match {
    case Some(c) => rulesForCell(lineNumber,c, columnDefinition)
    case _ => s"Missing value at line: ${lineNumber}, column: ${columnDefinition.id}".failNel[Any]
  }

  private def rulesForCell(lineNumber: Int, cell: String, columnDefinition: ColumnDefinition) = {
    columnDefinition.rules.map(_.execute(lineNumber, columnDefinition, cell)).sequence[({type x[a] = ValidationNEL[String, a]})#x, Any]
  }
}
