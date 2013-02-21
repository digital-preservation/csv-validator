package uk.gov.tna.dri.validator

import org.specs2.mutable.Specification
import scalaz._

class MetaDataValidatorBigFileSpec extends Specification {

  val basePath = "src/test/resources/uk/gov/tna/dri/validator/acceptance/"

  "Big file" should {

    "succeed with no stack overflow for all errors" in {
      val v = new MetaDataValidatorApp with AllErrorsMetaDataValidator
      v.validate(basePath + "bigMetaData.csv", basePath + "bigSchema.txt") must beLike {
        case Success(_) => ok
      }
    }

    "succeed with no stack overflow for fail fast" in {
      val v = new MetaDataValidatorApp with FailFastMetaDataValidator
      v.validate(basePath + "bigMetaData.csv", basePath + "bigSchema.txt") must beLike {
        case Success(_) => ok
      }
    }
  }
}