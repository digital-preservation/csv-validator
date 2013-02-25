package uk.gov.tna.dri.validator

import scalaz.Scalaz._
import java.io.Reader
import uk.gov.tna.dri.schema.{NoHeader, Schema}
import uk.gov.tna.dri.metadata.{Cell, Row}
import scala.collection.JavaConversions._

import au.com.bytecode.opencsv.CSVReader

trait MetaDataValidator {
  type MetaDataValidation[S] = ValidationNEL[String, S]

  def validate(csv: Reader, schema: Schema): MetaDataValidation[Any] = {
    val rows = new CSVReader(csv).readAll().toList
    if (rows.isEmpty) "metadata file is empty".failNel[Any] else {
      val rowsWithNoHeader = if (schema.globalDirectives.contains(NoHeader())) rows else rows.drop(1)
      if (rowsWithNoHeader.isEmpty) "metadata file has a header but no data".failNel[Any]
      else validateRows(rowsWithNoHeader.map(_.toList).zipWithIndex.map(r => Row(r._1.map(Cell(_)), r._2 + 1)), schema)
    }
  }

  def validateRows(rows: List[Row], schema: Schema): MetaDataValidation[Any]
}