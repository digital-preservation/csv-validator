package uk.gov.tna.dri.validator

import uk.gov.tna.dri.schema.{NoHeader, TotalColumns, Optional, Schema}
import au.com.bytecode.opencsv.CSVReader
import java.io.Reader
import scala.collection.JavaConversions._
import scalaz._
import Scalaz._
import uk.gov.tna.dri.metadata.{Cell, Row}

trait AllErrorsMetaDataValidator extends MetaDataValidator {

  def validate(csv: Reader, schema: Schema): MetaDataValidation[Any] = {

    def rowsWithHeadDirective(rows: List[Array[String]]): List[Array[String]] = {
      schema match {
        case Schema(globalDirectives, _) if globalDirectives.contains(NoHeader()) => rows
        case _ => rows.tail
      }
    }

    val rows = rowsWithHeadDirective(new CSVReader(csv).readAll().toList)
    val v: List[MetaDataValidation[Any]] = for ((row, rowIndex) <- rows.map(_.toList).zipWithIndex) yield validateRow(Row(row.map(Cell(_)), rowIndex + 1), schema)
    v.sequence[MetaDataValidation, Any]
  }

  private def validateRow(row: Row, schema: Schema): MetaDataValidation[Any] = {
    val totalColumnsV = totalColumns(row, schema)

    val rulesV = rules(row, schema)
    (totalColumnsV |@| rulesV) { _ :: _ }
  }

  private def totalColumns(row: Row, schema: Schema): MetaDataValidation[Any] = {
    val tc: Option[TotalColumns] = schema.globalDirectives.collectFirst{ case t@TotalColumns(_) => t }

    if (tc.isEmpty || tc.get.numberOfColumns == row.cells.length) true.successNel[String]
    else s"Expected @totalColumns of ${tc.get.numberOfColumns} and found ${row.cells.length} on line ${row.lineNumber}".failNel[Any]
  }

  private def rules(row: Row, schema: Schema): MetaDataValidation[List[Any]] = {
    val cells: (Int) => Option[Cell] = row.cells.lift
    val v = for { (columnDefinition, columnIndex) <- schema.columnDefinitions.zipWithIndex } yield validateCell(columnIndex, cells, row, schema)
    v.sequence[MetaDataValidation, Any]
  }

  private def validateCell(columnIndex: Int, cells: (Int) => Option[Cell], row: Row, schema: Schema): MetaDataValidation[Any] = {
    cells(columnIndex) match {
      case Some(c) => rulesForCell(columnIndex, row, schema)
      case _ => s"Missing value at line: ${row.lineNumber}, column: ${schema.columnDefinitions(columnIndex).id}".failNel[Any]
    }
  }

  private def rulesForCell(columnIndex: Int, row: Row, schema: Schema): MetaDataValidation[Any] = {
    val columnDefinition = schema.columnDefinitions(columnIndex)

    if (row.cells(columnIndex).value.trim.isEmpty && columnDefinition.directives.contains(Optional())) true.successNel
    else columnDefinition.rules.map(_.evaluate(columnIndex, row, schema)).sequence[MetaDataValidation, Any]
  }
}