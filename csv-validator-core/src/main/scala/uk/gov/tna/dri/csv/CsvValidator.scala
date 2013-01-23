package uk.gov.tna.dri.csv

import uk.gov.tna.dri.schema.Schema
import au.com.bytecode.opencsv.CSVReader
import java.io.StringReader
import scala.collection.JavaConversions._


object CsvValidator {
  def validate(csv: String, schema: Schema) = {
    val reader = new CSVReader(new StringReader(csv))
    reader.readAll.forall(row => row.length == schema.totalColumns)
  }
}
