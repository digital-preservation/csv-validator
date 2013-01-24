package uk.gov.tna.dri.csv

import org.specs2.mutable.Specification

class CsvValidatorAppSpec extends Specification {

  "App" should {

    "fail when incorrect number of arguments supplied" in {
      CsvValidatorApp.argumentCountValid(Array[String]("")) mustEqual false
      CsvValidatorApp.argumentCountValid(Array[String]("metaData")) mustEqual false
      CsvValidatorApp.argumentCountValid(Array[String]("metaData.csv", "schema.txt", "something extra")) mustEqual false
    }

    "give a usage message" in {
      CsvValidatorApp.usage mustEqual "Usage: validate <meta-data file path> <schema file path>"
    }

    "check for correct number of arguments" in {
      CsvValidatorApp.argumentCountValid(Array[String]("metaDataFile", "schemaFile")) mustEqual true
    }

    "give the meta data file from first argument and schema file from second argument" in {
      CsvValidatorApp.inputFilePaths(Array[String]("metaDataFile.csv", "schema.txt")) mustEqual("metaDataFile.csv", "schema.txt")
    }

    "fail for unreadable input file" in {
      CsvValidatorApp.fileReadable("/some/non/existant/file") mustEqual false
    }

    "succeed for a readable input file" in {
      CsvValidatorApp.fileReadable("src/test/resources/uk/gov/tna/dri/csv/metaData.csv") mustEqual true
    }

    "give a file not readable message for file path" in {
      CsvValidatorApp.fileNotReadableMessage("no/file/here.txt") mustEqual "Unable to read file : no/file/here.txt"
    }
  }
}
