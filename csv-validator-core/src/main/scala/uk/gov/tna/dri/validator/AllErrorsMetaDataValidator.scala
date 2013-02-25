package uk.gov.tna.dri.validator

import uk.gov.tna.dri.schema.{TotalColumns, Optional, Schema}
import scalaz._
import Scalaz._
import uk.gov.tna.dri.metadata.{Cell, Row}

trait AllErrorsMetaDataValidator extends MetaDataValidator {

  def validateRows(rows: List[Row], schema: Schema): MetaDataValidation[Any] = {
    val v = for (row <- rows) yield validateRow(row, schema)
    v.sequence[MetaDataValidation, Any]
  }

  private def validateRow(row: Row, schema: Schema): MetaDataValidation[Any] = {
    val totalColumnsV = totalColumns(row, schema)
    val rulesV = rules(row, schema)
    (totalColumnsV |@| rulesV) { _ :: _ }
  }

  private def totalColumns(row: Row, schema: Schema): MetaDataValidation[Any] = {
    val tc: Option[TotalColumns] = schema.globalDirectives.collectFirst {
      case t@TotalColumns(_) => t
    }

    if (tc.isEmpty || tc.get.numberOfColumns == row.cells.length) true.successNel[String]
    else s"Expected @totalColumns of ${tc.get.numberOfColumns} and found ${row.cells.length} on line ${row.lineNumber}".failNel[Any]
  }

  private def rules(row: Row, schema: Schema): MetaDataValidation[List[Any]] = {
    val cells: (Int) => Option[Cell] = row.cells.lift
    val v = for {(columnDefinition, columnIndex) <- schema.columnDefinitions.zipWithIndex} yield validateCell(columnIndex, cells, row, schema)
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