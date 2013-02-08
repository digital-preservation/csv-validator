package uk.gov.tna.dri.validator

import org.specs2.mutable.Specification
import scalaz._

class MetaDataValidatorAppSpec extends Specification {

  val basePath = "src/test/resources/uk/gov/tna/dri/validator/"

  "Argument check" should {
    "give usage message when no arguments supplied" in {
      MetaDataValidatorApp.checkArguments(Nil) must beLike { case Failure(errors) => errors.head mustEqual "Usage: validate <meta-data file path> <schema file path>" }
    }

    "give usage message when one argument supplied" in {
      MetaDataValidatorApp.checkArguments(List("meta file")) must beLike { case Failure(errors) => errors.head mustEqual "Usage: validate <meta-data file path> <schema file path>"}
    }

    "give usage message when too many arguments supplied" in {
      MetaDataValidatorApp.checkArguments(List("somMetaData.csv", "someSchema.txt", "something extra")) must beLike { case Failure(errors) => errors.head mustEqual "Usage: validate <meta-data file path> <schema file path>" }
    }

    "fail if metadata file is unreadable" in {
      MetaDataValidatorApp.checkArguments(List("nonExistentMetaData.csv", basePath + "schema.txt")) must beLike {
        case Failure(errors) => errors.head mustEqual "Unable to read file : nonExistentMetaData.csv"
      }
    }

    "fail if schema file is unreadable" in {
      MetaDataValidatorApp.checkArguments(List(basePath + "metaData.csv", "nonExistentSchema.txt")) must beLike {
        case Failure(errors) =>  errors.head mustEqual "Unable to read file : nonExistentSchema.txt"
      }
    }

    "fail if both metadata and schema file are unreadable" in {
      MetaDataValidatorApp.checkArguments(List("nonExistentmetaData.csv", "nonExistentSchema.txt")) must beLike {
        case Failure(errors) =>  errors.list must contain ("Unable to read file : nonExistentmetaData.csv", "Unable to read file : nonExistentSchema.txt")
      }
    }

    "succeed if both metadata and schema file are readable" in {
      MetaDataValidatorApp.checkArguments(List(basePath + "metaData.csv", basePath + "schema.txt")) must beLike {
        case Success(_) => ok
      }
    }
  }

  "Validation" should {

    "fail for invalid schema" in {
      MetaDataValidatorApp.validate(basePath + "metaData.csv", basePath + "badSchema.txt") must beLike {
        case Failure(errors) => errors.head mustEqual s"Schema Parse Error: @TotalColumns invalid at line 1, column 1"
      }
    }

    "succeed for valid schema and metadata file" in {
      MetaDataValidatorApp.validate(basePath + "metaData.csv", basePath + "schema.txt") must beLike {
        case Success(_) => ok
      }
    }
  }
}
