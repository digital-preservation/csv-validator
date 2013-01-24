package uk.gov.tna.dri.csv

import uk.gov.tna.dri.schema.{SchemaParser, Schema}
import au.com.bytecode.opencsv.CSVReader
import java.io.Reader
import scala.collection.JavaConversions._

trait CsvValidator {
  this: SchemaParser =>

  def validate(csv: Reader, schema: Reader) = {
    val reader = new CSVReader(csv)

    parse(schema) match {
      case Success(s: Schema, _) => reader.readAll.forall(row => row.length == s.totalColumns)
      case _ => false
    }
  }
}