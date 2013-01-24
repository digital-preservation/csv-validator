package uk.gov.tna.dri.csv

import org.specs2.mutable.Specification
import uk.gov.tna.dri.schema.{SchemaParser, Schema}

class CsvValidatorSpec extends Specification {

  val validator = new CsvValidator with SchemaParser

  "Validation" should {
    "succeed for correct total columns" in {

      validator.validate("col1", "{ @TotalColumns 1 }") mustEqual true
    }

    "fail for incorrect number of total columns" in {
      validator.validate("col1, col2", "{ @TotalColumns 1 }") mustEqual false
    }

    "succeed for correct total columns for multiple lines" in {
      val csv =
        """col1, col2
           col1, col2"""

      validator.validate(csv, "{ @TotalColumns 2 }") mustEqual true
    }

    "fail for incorrect number of total columns for multiple lines" in {
      val csv =
        """col1, col2, col3
           col1, col2
           col1, col2, col3"""
      validator.validate(csv, "{ @TotalColumns 3 }") mustEqual false
    }
  }
}
