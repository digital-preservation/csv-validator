package uk.gov.tna.dri.validator

import org.specs2.mutable.Specification

class MetaDataValidatorAppSpec extends Specification {

  "App" should {

    "fail when incorrect number of arguments supplied" in {
      MetaDataValidatorApp.argumentCountValid(Array[String]("")) mustEqual false
      MetaDataValidatorApp.argumentCountValid(Array[String]("metaData")) mustEqual false
      MetaDataValidatorApp.argumentCountValid(Array[String]("metaData.csv", "schema.txt", "something extra")) mustEqual false
    }

    "give a usage message" in {
      MetaDataValidatorApp.usage mustEqual "Usage: validate <meta-data file path> <schema file path>"
    }

    "check for correct number of arguments" in {
      MetaDataValidatorApp.argumentCountValid(Array[String]("metaDataFile", "schemaFile")) mustEqual true
    }

    "give the meta data file from first argument and schema file from second argument" in {
      MetaDataValidatorApp.inputFilePaths(Array[String]("metaDataFile.csv", "schema.txt")) mustEqual("metaDataFile.csv", "schema.txt")
    }

    "fail for unreadable input file" in {
      MetaDataValidatorApp.fileReadable("/some/non/existant/file") mustEqual false
    }

    "succeed for a readable input file" in {
      MetaDataValidatorApp.fileReadable("src/test/resources/uk/gov/tna/dri/validator/metaData.csv") mustEqual true
    }

    "give a file not readable message for file path" in {
      MetaDataValidatorApp.fileNotReadableMessage("no/file/here.txt") mustEqual "Unable to read file : no/file/here.txt"
    }
  }
}
