package uk.gov.tna.dri.validator

import uk.gov.tna.dri.schema.{ColumnDefinition, RegexRule, Schema}
import au.com.bytecode.opencsv.CSVReader
import java.io.Reader
import scala.collection.JavaConversions._
import scalaz._
import Scalaz._

trait MetaDataValidator {

  def validate(csv: Reader, schema: Schema) = {
    val rows = new CSVReader(csv).readAll() toList

    val totalColsValidation = totalColumns(rows, schema)
    val rowsValidation =  validateRows(rows.map(_.toList), schema)

    (totalColsValidation |@| rowsValidation) tupled
  }

  def totalColumns(rows: List[Array[String]], schema: Schema) = {
    rows.zipWithIndex.find(r => r._1.length != schema.totalColumns) match {
      case Some((row, rowIndex)) => s"Expected @TotalColumns of ${schema.totalColumns} and found ${row.length} on line ${rowIndex + 1}".failNel[Boolean]
      case _ => true.successNel[String]
    }
  }

  def validateRows(rows: List[List[String]], schema: Schema) = {
    val validations = for {(row, rowIndex) <- rows.zipWithIndex} yield (validateRow(rowIndex, row, schema.columnDefinitions))
    validations.sequence[({type x[a] = ValidationNEL[String, a]})#x, List[Boolean]]
  }

  private def validateRow(rowIndex: Int, row: List[String], columnDefinitions: List[ColumnDefinition]) = {
    val valueWithColumnDefinition = row.zip(columnDefinitions)
    val validations =
      for { (value, columnDef) <- valueWithColumnDefinition
             rule <- columnDef.rules
        } yield rule.execute(rowIndex, columnDef, value)

    validations.sequence[({type x[a] = ValidationNEL[String, a]})#x, Boolean]
   }
}
