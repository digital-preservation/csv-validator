package uk.gov.tna.dri.csv

import org.specs2.mutable.Specification
import uk.gov.tna.dri.schema.SchemaParser
import java.io.StringReader

class CsvValidatorSpec extends Specification {

  implicit def stringToStringReader(text: String) = new StringReader(text)

  val csvValidator = new CsvValidator with SchemaParser

  "Validation" should {
    "succeed for correct total columns" in {
      csvValidator.validate("col1", "{ @TotalColumns 1 }") mustEqual true
    }

    "fail for incorrect number of total columns" in {
      csvValidator.validate("col1, col2", "{ @TotalColumns 1 }") mustEqual false
    }

    "succeed for correct total columns for multiple lines" in {
      val csv =
        """col1, col2
           col1, col2"""

      csvValidator.validate(csv, "{ @TotalColumns 2 }") mustEqual true
    }

    "fail for incorrect number of total columns for multiple lines" in {
      val csv =
        """col1, col2, col3
           col1, col2
           col1, col2, col3"""

      csvValidator.validate(csv, "{ @TotalColumns 3 }") mustEqual false
    }
  }
}
