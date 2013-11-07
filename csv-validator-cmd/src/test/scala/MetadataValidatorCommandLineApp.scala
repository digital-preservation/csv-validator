package uk.gov.tna.dri.csv.validator.cmd

import scala.Array
import org.specs2.mutable.Specification
import uk.gov.tna.dri.csv.validator.EOL

class CsvValidatorCmdAppSpec extends Specification {

  val basePath = "src/test/resources/uk/gov/tna/dri/validator/"

  "Check arguments" should {
    "give usage message when no arguments supplied" in {
      CsvValidatorCmdApp.run(Array.empty) must beLike {
        case (errMsg,errCode) => errMsg mustEqual """Usage: validate [--fail-fast] [--path <from> <to>]* <meta-data file path> <uk.gov.tna.dri.csv.validator.schema file path>"""
      }
    }

    "give usage message when one argument supplied" in {
      CsvValidatorCmdApp.run(Array("meta file")) must beLike {
        case (errMsg,errCode) => errMsg mustEqual """Usage: validate [--fail-fast] [--path <from> <to>]* <meta-data file path> <uk.gov.tna.dri.csv.validator.schema file path>"""
      }
    }

    //    "give usage message when too many arguments supplied" in {
    //      CsvValidatorCmdApp.run(List("somMetaData.csv", "someSchema.txt", "something extra").toArray) must beLike {
    //        case (errMsg,errCode) => errMsg mustEqual """Usage: validate [--fail-fast] [--path <from> <to>]* <meta-data file path> <uk.gov.tna.dri.csv.validator.schema file path>"""
    //      }
    //    }

    "fail if metadata file is unreadable" in {
      CsvValidatorCmdApp.run(Array("nonExistentMetaData.csv", basePath + "uk.gov.tna.dri.csv.validator.schema.txt")) must beLike {
        case (errMsg,errCode) => errMsg mustEqual "Unable to read file : nonExistentMetaData.csv"
      }
    }

    "fail if uk.gov.tna.dri.csv.validator.schema file is unreadable" in {
      CsvValidatorCmdApp.run(Array(basePath + "metaData.csv", "nonExistentSchema.txt")) must beLike {
        case (errMsg,errCode) => errMsg mustEqual "Unable to read file : nonExistentSchema.txt"
      }
    }

    "fail if both metadata and uk.gov.tna.dri.csv.validator.schema file are unreadable" in {
      CsvValidatorCmdApp.run(Array("nonExistentmetaData.csv", "nonExistentSchema.txt")) must beLike {
        case (errMsg,errCode) => errMsg mustEqual "Unable to read file : nonExistentmetaData.csv" + EOL + "Unable to read file : nonExistentSchema.txt"
      }
    }

    "succeed if both metadata and uk.gov.tna.dri.csv.validator.schema file are readable" in {
      CsvValidatorCmdApp.run(Array(basePath + "metaData.csv", basePath + "uk.gov.tna.dri.csv.validator.schema.txt")) must beLike {
        case (errMsg,errCode) => errMsg mustEqual "PASS"
      }
    }
  }

  "Fail fast and file args" should {

    "return true and the file names for fail fast" in {
      CsvValidatorCmdApp.run(Array("--fail-fast", "someMetaData.csv", "someSchema.txt")) mustEqual ("Unable to read file : someMetaData.csv" + EOL + "Unable to read file : someSchema.txt",1)
    }

    "return true and the file names for fail fast short form" in {
      CsvValidatorCmdApp.run(Array("-f", "someMetaData.csv", "someSchema.txt")) mustEqual ("Unable to read file : someMetaData.csv" + EOL + "Unable to read file : someSchema.txt",1)
    }

    "return false and the file names for no fail fast" in {
      CsvValidatorCmdApp.run(Array("someMetaData.csv", "someSchema.txt")) mustEqual ("Unable to read file : someMetaData.csv" + EOL + "Unable to read file : someSchema.txt",1)
    }
  }

  "--path and file args" in {
    //    "find " {
    //
    //    }

    //    "handle --path option" in {
    //      val commandLine = List[String](
    //        "--path", "c:", "",
    //        "--path", """file://c:""", """file://""",
    //        "--fail-fast"
    //      )
    //      CsvValidatorCmdApp.findSubstitutionPaths(commandLine) mustEqual Success(List( ("c:", ""), ("file://c:", "file://")) )
    //    }
    pending
  }

  "Given a list of args" should {
    "be able to find all the paths" in {
      CsvValidatorCmdApp.findPaths( List("--path", "hello", "world")) mustEqual Right( (List( ("hello", "world") ), List()))
    }

    "be able to find the path and return remainders" in {
      CsvValidatorCmdApp.findPaths( List("123", "--path", "hello", "world", "xyz")) mustEqual Right( (List( ("hello", "world") ), List("123", "xyz")))
    }

    "be able to find multiple paths and return remainders" in {
      CsvValidatorCmdApp.findPaths( List("123", "--path", "hello", "world", "xyz", "--path", "hello2", "world2")) mustEqual Right( (List( ("hello", "world"), ("hello2", "world2") ), List("123", "xyz")))
    }


    "be able to handle a missing value" in {
      CsvValidatorCmdApp.findPaths( List("--path", "hello" )) mustEqual Left("Missing param to --path" + EOL + "Usage: validate [--fail-fast] [--path <from> <to>]* <meta-data file path> <uk.gov.tna.dri.csv.validator.schema file path>")
    }

    "find the --fail-fast option" in {
      CsvValidatorCmdApp.findFailFast( List("--fail-fast", "hello", "world")) mustEqual Right( true, List("hello", "world"))
    }

    "find find the metafile and uk.gov.tna.dri.csv.validator.schema" in {
      CsvValidatorCmdApp.findFiles( List("--fail-fast", basePath + "metaData.csv", basePath + "uk.gov.tna.dri.csv.validator.schema.txt")) mustEqual Right( (basePath + "metaData.csv", basePath + "uk.gov.tna.dri.csv.validator.schema.txt"), List("--fail-fast"))
    }
  }


  "Command line app" should {

    "have exit code 0 when validation successful" in {
      CsvValidatorCmdApp.run(Array(basePath + "metaData.csv", basePath + "uk.gov.tna.dri.csv.validator.schema.txt")) mustEqual Tuple2("PASS", 0)
    }

    //    "have exit code 0 when validation --path successful" in {
    //      CsvValidatorCmdApp.run(Array( "--path", "c:", "", basePath + "metaData.csv", basePath + "uk.gov.tna.dri.csv.validator.schema.txt"))._2 mustEqual 0
    //    }

    "have exit code 1 when the command line arguments are wrong" in {
      CsvValidatorCmdApp.run(Array(""))._2 mustEqual 1
    }

    "have exit code 1 when the --path is missing one args " in {
      CsvValidatorCmdApp.run(Array("--path", "c:", basePath + "metaData.csv", basePath + "uk.gov.tna.dri.csv.validator.schema.txt"))._2 mustEqual 1
    }

    "have exit code 1 when the --path is missing both args " in {
      CsvValidatorCmdApp.run(Array("--path", basePath + "metaData.csv", basePath + "uk.gov.tna.dri.csv.validator.schema.txt"))._2 mustEqual 1
    }


    "have exit code 2 when the uk.gov.tna.dri.csv.validator.schema is invalid" in {
      CsvValidatorCmdApp.run(Array(basePath + "metaData.csv", basePath + "badSchema.txt"))._2 mustEqual 2
    }

    "have exit code 3 when the metadata is invalid" in {
      CsvValidatorCmdApp.run(Array(basePath + "acceptance/standardRulesFailMetaData.csv", basePath + "acceptance/standardRulesSchema.txt"))._2 mustEqual 3
    }

  }
}