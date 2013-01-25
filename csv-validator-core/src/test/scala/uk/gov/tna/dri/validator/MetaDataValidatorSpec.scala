package uk.gov.tna.dri.validator

import org.specs2.mutable.Specification
import uk.gov.tna.dri.schema.{Schema, SchemaParser}
import java.io.StringReader

class MetaDataValidatorSpec extends Specification {

  object TestMetaDataValidator extends MetaDataValidator

  "Validation" should {
    "succeed for correct total columns" in {
      val metaData = "col1"
      TestMetaDataValidator.validate(new StringReader(metaData), Schema(1)) mustEqual true
    }

    "fail for incorrect number of total columns" in {
      val metaData = "col1, col2"
      TestMetaDataValidator.validate(new StringReader(metaData), Schema(1)) mustEqual false
    }

    "succeed for correct total columns for multiple lines" in {
      val metaData =
        """col1, col2
           col1, col2"""

      TestMetaDataValidator.validate(new StringReader(metaData), Schema(2)) mustEqual true
    }

    "fail for incorrect number of total columns for multiple lines" in {
      val metaData =
        """col1, col2, col3
           col1, col2
           col1, col2, col3"""

      TestMetaDataValidator.validate(new StringReader(metaData), Schema(3)) mustEqual false
    }
  }
}
