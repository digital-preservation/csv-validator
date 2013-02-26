package uk.gov.tna.dri.validator

import org.specs2.mutable.Specification
import scalaz._
import uk.gov.tna.dri.schema.Schema

class MetaDataValidatorBigFileSpec extends Specification {

  val basePath = "src/test/resources/uk/gov/tna/dri/validator/acceptance/"

  "Big file" should {

    "succeed with no stack overflow for all errors" in {
      val v = new MetaDataValidatorApp with AllErrorsMetaDataValidator
      def getSchema(filePath: String): Schema = {
        v.parseSchema(filePath) match {
          case Success(s) => s
          case _  => throw new RuntimeException("Unable to parse schema")
        }
      }
      v.validate(basePath + "bigMetaData.csv", getSchema(basePath + "bigSchema.txt")) must beLike {
        case Success(_) => ok
      }
    }

    "succeed with no stack overflow for fail fast" in {
      val v = new MetaDataValidatorApp with FailFastMetaDataValidator
      def getSchema(filePath: String): Schema = {
        v.parseSchema(filePath) match {
          case Success(s) => s
          case _  => throw new RuntimeException("Unable to parse schema")
        }
      }
      v.validate(basePath + "bigMetaData.csv", getSchema(basePath + "bigSchema.txt")) must beLike {
        case Success(_) => ok
      }
    }
  }
}