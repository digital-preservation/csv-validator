package uk.gov.tna.dri.csv

import uk.gov.tna.dri.schema.{SchemaParser, Schema}
import au.com.bytecode.opencsv.CSVReader
import java.io.StringReader
import scala.collection.JavaConversions._


trait CsvValidator {
  this: SchemaParser =>

  def validate(csv: String, schemaText: String) = {
    val reader = new CSVReader(new StringReader(csv))
    parse(schemaText) match {
      case Success(s: Schema, _) => reader.readAll.forall(row => row.length == s.totalColumns)
      case _ => false
    }

  }
}
