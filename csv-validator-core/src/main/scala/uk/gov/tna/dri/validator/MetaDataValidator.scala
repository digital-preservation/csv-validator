package uk.gov.tna.dri.validator

import uk.gov.tna.dri.schema.{Optional, Schema}
import au.com.bytecode.opencsv.CSVReader
import java.io.Reader
import scala.collection.JavaConversions._
import scalaz._
import Scalaz._
import uk.gov.tna.dri.metadata.{Cell, Row}

trait MetaDataValidator {

  type MetaDataValidation[S] = ValidationNEL[String, S]

  def validate(csv: Reader, schema: Schema, failFast: Boolean) = {
    val rows = new CSVReader(csv).readAll()
    var cont = true
    val v = for {
      (row, rowIndex) <- rows.zipWithIndex
      if (cont)
         validatedRow = validateRow(Row(row.toList.map(Cell(_)), rowIndex + 1), schema)
      dummyValNotUsed = validatedRow match {
      case Success (xv) => {
        true
      }
      case f => {
        if (failFast)
          cont = false
        false
      }
      }

    } yield validatedRow
    v.sequence[MetaDataValidation, Any]
  }

  private def validateRow(row: Row, schema: Schema) = {
    val totalColumnsV = totalColumns(row, schema)

    val rulesV = rules(row, schema)
    (totalColumnsV |@| rulesV) { _ :: _ }
  }

  private def totalColumns(row: Row, schema: Schema) = {
    if (row.cells.length == schema.totalColumns) true.successNel[String]
    else s"Expected @TotalColumns of ${schema.totalColumns} and found ${row.cells.length} on line ${row.lineNumber}".failNel[Any]
  }

  private def rules(row: Row, schema: Schema) = {
    val v = for { (columnDefinition, columnIndex) <- schema.columnDefinitions.zipWithIndex } yield validateCell(columnIndex, row, schema)
    v.sequence[MetaDataValidation, Any]
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

    if (row.cells(columnIndex).value.trim.isEmpty && columnDefinition.contains(Optional())) {println("cell ok");true.successNel }
    else {
      println("executing rules for " + columnDefinition.rules)
      columnDefinition.rules.map(_.execute(columnIndex, row, schema)).sequence[MetaDataValidation, Any]
    }
  }
}