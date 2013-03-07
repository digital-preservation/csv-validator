package uk.gov.tna.dri.validator

import org.specs2.mutable.Specification
import scalaz._
import uk.gov.tna.dri.schema.Schema

class MetaDataValidatorBusinessAcceptanceSpec extends Specification {

  val basePath = "src/test/resources/uk/gov/tna/dri/validator/acceptance/dptests/"

  val v: MetaDataValidatorApp = new MetaDataValidatorApp with AllErrorsMetaDataValidator { val pathSubstitutions = List[(String,String)]() }
  import v.{validate, parseSchema}

  def parse(filePath: String): Schema = parseSchema(filePath) fold (f => throw new IllegalArgumentException(f.toString()), s => s)

  "Regex rule" should {

    "succeed" in {
      validate(basePath + "regexRulePassMetaData.csv", parse(basePath + "regexRuleSchema.txt")) must beLike {
        case Success(_) => ok
      }
    }

    "fail" in {
      validate(basePath + "regexRuleFailMetaData.csv", parse(basePath + "regexRuleSchema.txt")) must beLike {
        case Failure(_) => ok
      }
    }
  }

}