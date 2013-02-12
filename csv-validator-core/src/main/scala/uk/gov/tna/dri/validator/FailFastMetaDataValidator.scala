package uk.gov.tna.dri.validator

import uk.gov.tna.dri.schema.{Optional, Schema}
import au.com.bytecode.opencsv.CSVReader
import java.io.Reader
import scala.collection.JavaConversions._
import scalaz._
import Scalaz._
import uk.gov.tna.dri.metadata.{Cell, Row}

trait FailFastMetaDataValidator {

  type FailFastMetaDataValidation[S] = ValidationNEL[String, S]

  def validateFailFast(csv: Reader, schema: Schema) = {
    val csvRows = new CSVReader(csv).readAll()
    val rows = csvRows.toList.zipWithIndex.map(r => Row(r._1.toList.map(Cell(_)), r._2 + 1))

    def validateRows(rows: List[Row]): FailFastMetaDataValidation[Any] = rows match {
      case r :: tail =>  validateRow(r,schema).fold(e => e.fail[Any], s => validateRows(tail))
      case Nil => true.successNel[String]
    }

    validateRows(rows)
  }


  private def validateRow(row: Row, schema: Schema): FailFastMetaDataValidation[Any] = {
    totalColumns(row, schema)
  }

  private def totalColumns(row: Row, schema: Schema) = {
    if (row.cells.length == schema.totalColumns) true.successNel[String]
    else s"Expected @TotalColumns of ${schema.totalColumns} and found ${row.cells.length} on line ${row.lineNumber}".failNel[Any]
  }

  private def rules(row: Row, schema: Schema) = {
    val v = for { (columnDefinition, columnIndex) <- schema.columnDefinitions.zipWithIndex } yield validateCell(columnIndex, row, schema)
    v.sequence[FailFastMetaDataValidation, Any]
  }

  private def validateCell(columnIndex: Int, row: Row, schema: Schema) = {
    val cells = row.cells.lift

    cells(columnIndex) match {
      case Some(c) => rulesForCell(columnIndex, row, schema)
      case _ => s"Missing value at line: ${row.lineNumber}, column: ${schema.columnDefinitions(columnIndex).id}".failNel[Any]
    }
  }

  private def rulesForCell(columnIndex: Int, row: Row, schema: Schema) = {
    val columnDefinition = schema.columnDefinitions(columnIndex)

    if (row.cells(columnIndex).value.trim.isEmpty && columnDefinition.contains(Optional())) true.successNel
    else columnDefinition.rules.map(_.execute(columnIndex, row, schema)).sequence[FailFastMetaDataValidation, Any]
  }
}