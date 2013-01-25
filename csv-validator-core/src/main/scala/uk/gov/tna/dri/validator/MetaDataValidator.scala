package uk.gov.tna.dri.validator

import uk.gov.tna.dri.schema.Schema
import au.com.bytecode.opencsv.CSVReader
import java.io.Reader
import scala.collection.JavaConversions._

trait MetaDataValidator {
  def validate(csv: Reader, schema: Schema) = new CSVReader(csv).readAll.forall(row => row.length == schema.totalColumns)
}