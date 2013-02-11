package uk.gov.tna.dri.validator

import org.specs2.mutable.Specification
import scalaz._

class MetaDataValidatorAcceptanceSpec extends Specification {

  val basePath = "src/test/resources/uk/gov/tna/dri/validator/acceptance/"

  "Regex rule" should {

    "succeed for metadata file with column that passes regex rule" in {
      MetaDataValidatorApp.validate(basePath + "regexRulePassMetaData.csv", basePath + "regexRuleSchema.txt") must beLike {
        case Success(_) => ok
      }
    }

    "fail with line number and column id in error message " in {
      MetaDataValidatorApp.validate(basePath + "regexRuleFailMetaData.csv", basePath + "regexRuleSchema.txt") must beLike {
        case Failure(errors) => errors.head mustEqual "regex: [0-9]+ fails for line 1, column: Age"
      }
    }
  }

  "Multiple errors " should {
    "all be reported" in {
      MetaDataValidatorApp.validate(basePath + "multipleErrorsMetaData.csv", basePath + "regexRuleSchema.txt") must beLike {
        case Failure(errors) => errors.list must contain("regex: [0-9]+ fails for line 1, column: Age", "regex: [0-9]+ fails for line 2, column: Age")
      }
    }
  }
}
