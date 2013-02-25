package uk.gov.tna.dri.validator

import org.specs2.mutable.Specification
import scalaz._
import uk.gov.tna.dri.schema.Schema

class MetaDataValidatorBusinessAcceptanceSpec extends Specification {

  val basePath = "src/test/resources/uk/gov/tna/dri/validator/acceptance/dptests/"

  val v: MetaDataValidatorApp = new MetaDataValidatorApp with AllErrorsMetaDataValidator
  import v.{validate, parseSchema}
  def getSchema(filePath: String): Schema = {
    parseSchema(filePath) match {
      case Success(s) => s
    }
  }
  "Regex rule" should {

    "succeed" in {
      validate(basePath + "regexRulePassMetaData.csv", getSchema(basePath + "regexRuleSchema.txt")) must beLike {
        case Success(_) => ok
      }
    }

    "fail" in {
      validate(basePath + "regexRuleFailMetaData.csv", getSchema(basePath + "regexRuleSchema.txt")) must beLike {
        case Failure(_) => ok
      }
    }
  }

}