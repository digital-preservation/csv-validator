package uk.gov.tna.dri.validator

import org.specs2.mutable.Specification
import scalaz._

class MetaDataValidatorBigFileSpec extends Specification {

  val basePath = "src/test/resources/uk/gov/tna/dri/validator/acceptance/"

  val v: MetaDataValidatorApp = new MetaDataValidatorApp with AllErrorsMetaDataValidator
  import v.validate
  
  "Big file" should {

    "succeed with no stack overflow" in {
      validate(basePath + "bigMetaData.csv", basePath + "bigSchema.txt") must beLike {
        case Success(_) => ok
      }
    }
  }
}