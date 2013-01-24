package uk.gov.tna.dri.validator

import org.specs2.mutable.Specification
import uk.gov.tna.dri.schema.{Schema, SchemaParser}
import java.io.FileReader

class MetaDataValidatorIntegrationSpec extends Specification {
   val validator = new MetaDataValidator with SchemaParser

  "Validator" should {

    "validate metaData file against schema file" in {
      val schemaReader = new FileReader("src/test/resources/uk/gov/tna/dri/validator/schema.txt")
      val metaDataReader = new FileReader("src/test/resources/uk/gov/tna/dri/validator/metaData.csv")
      validator.validate(metaDataReader, schemaReader) mustEqual true
    }

  }
}