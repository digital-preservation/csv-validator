package uk.gov.tna.dri.validator

import uk.gov.tna.dri.schema._
import au.com.bytecode.opencsv.CSVReader
import java.io.Reader
import scala.collection.JavaConversions._
import scalaz._
import Scalaz._
import uk.gov.tna.dri.metadata.{Cell, Row}
import uk.gov.tna.dri.metadata.Cell
import uk.gov.tna.dri.metadata.Row
import uk.gov.tna.dri.schema.ColumnDefinition
import uk.gov.tna.dri.schema.Schema
import scala.Some
import uk.gov.tna.dri.schema.Optional

trait FailFastMetaDataValidator extends MetaDataValidator {

  def validate(csv: Reader, schema: Schema) = {

    def rowsWithHeadDirective(rows: List[Array[String]]): List[Array[String]] = {
      schema match {
        case Schema(_, _) => rows
        case _ => rows.tail
      }
    }

    val csvRows = rowsWithHeadDirective(new CSVReader(csv).readAll().toList)

    val rows = csvRows.map(_.toList).zipWithIndex.map(r => Row(r._1.map(Cell(_)), r._2 + 1))

    def validateRows(rows: List[Row]): MetaDataValidation[Any] = rows match {
      case r :: tail =>  validateRow(r, schema).fold(e => e.fail[Any], s => validateRows(tail))
      case Nil => true.successNel[String]
    }

    validateRows(rows)
  }

  private def validateRow(row: Row, schema: Schema): MetaDataValidation[Any] = {
    totalColumns(row, schema).fold(e => e.fail[Any], s => rules(row, schema))
  }

  private def totalColumns(row: Row, schema: Schema) = {
    val tc: Option[TotalColumns] = schema.globalDirectives.collectFirst{ case t@TotalColumns(_) => t }

    if (tc.isEmpty || tc.get.numberOfColumns == row.cells.length) true.successNel[String]
    else s"Expected @totalColumns of ${tc.get.numberOfColumns} and found ${row.cells.length} on line ${row.lineNumber}".failNel[Any]
  }

  private def rules(row: Row, schema: Schema): MetaDataValidation[Any] = {
    def rulesRecur(columnDefinitions:List[(ColumnDefinition,Int)]): MetaDataValidation[Any] = columnDefinitions match {
      case Nil => true.successNel[String]
      case (columnDef, columnIndex) :: tail => validateCell(columnIndex, row, schema).fold(e => e.fail[Any], s => rulesRecur(tail) )
    }
    rulesRecur(schema.columnDefinitions.zipWithIndex)
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

    def rulesForCellRecur(rules:List[Rule]): MetaDataValidation[Any] = rules match {
      case Nil => true.successNel[String]
      case rule :: tail => rule.evaluate(columnIndex, row, schema).fold(e => e.fail[Any], s => rulesForCellRecur(tail) )
    }

    if (row.cells(columnIndex).value.trim.isEmpty && columnDefinition.directives.contains(Optional())) true.successNel
    else rulesForCellRecur(columnDefinition.rules)
  }
}