package uk.gov.tna.dri.csv

import org.specs2.mutable.Specification
import uk.gov.tna.dri.schema.{Schema, SchemaParser}
import java.io.FileReader

class CsvValidatorIntegrationSpec extends Specification {
   val validator = new CsvValidator with SchemaParser

  "Validator" should {

    "validate metaData file against schema file" in {
      val schemaReader = new FileReader("src/test/resources/uk/gov/tna/dri/csv/schema.txt")
      val metaDataReader = new FileReader("src/test/resources/uk/gov/tna/dri/csv/metaData.csv")
      validator.validate(metaDataReader, schemaReader) mustEqual true
    }
  }
}