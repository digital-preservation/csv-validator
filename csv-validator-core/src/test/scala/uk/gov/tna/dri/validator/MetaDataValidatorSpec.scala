package uk.gov.tna.dri.validator

import org.specs2.mutable.Specification
import uk.gov.tna.dri.schema.SchemaParser
import java.io.StringReader

class MetaDataValidatorSpec extends Specification {

  implicit def stringToStringReader(text: String) = new StringReader(text)

  val validator = new MetaDataValidator with SchemaParser

  "Validation" should {
    "succeed for correct total columns" in {
      validator.validate("col1", "@TotalColumns 1") mustEqual true
    }

    "fail for incorrect number of total columns" in {
      validator.validate("col1, col2", "@TotalColumns 1") mustEqual false
    }

    "succeed for correct total columns for multiple lines" in {
      val metaData =
        """col1, col2
           col1, col2"""

      validator.validate(metaData, "@TotalColumns 2") mustEqual true
    }

    "fail for incorrect number of total columns for multiple lines" in {
      val metaData =
        """col1, col2, col3
           col1, col2
           col1, col2, col3"""

      validator.validate(metaData, "@TotalColumns 3") mustEqual false
    }
  }
}
