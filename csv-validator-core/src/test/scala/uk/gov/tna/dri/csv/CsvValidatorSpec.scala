package uk.gov.tna.dri.csv

import org.specs2.mutable.Specification
import uk.gov.tna.dri.schema.Schema

class CsvValidatorSpec extends Specification {

  "Validation" should {
    "succeed for correct total columns" in {
      CsvValidator.validate("col1", Schema(totalColumns = 1)) mustEqual true
    }

    "fail for incorrect number of total columns" in {
      CsvValidator.validate("col1, col2", Schema(totalColumns = 1)) mustEqual false
    }

    "succeed for correct total columns for multiple lines" in {
      val csv =
        """col1, col2
           col1, col2"""

      CsvValidator.validate(csv, Schema(totalColumns = 2)) mustEqual true
    }

    "fail for incorrect number of total columns for multiple lines" in {
      val csv =
        """col1, col2, col3
           col1, col2
           col1, col2, col3"""
      CsvValidator.validate(csv, Schema(totalColumns = 3)) mustEqual false
    }
  }
}
