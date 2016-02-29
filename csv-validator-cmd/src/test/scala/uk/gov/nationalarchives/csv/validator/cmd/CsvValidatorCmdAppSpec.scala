/**
 * Copyright (c) 2013, The National Archives <digitalpreservation@nationalarchives.gov.uk>
 * http://www.nationalarchives.gov.uk
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package uk.gov.nationalarchives.csv.validator.cmd

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CsvValidatorCmdAppSpec extends Specification with TestResources {

  val schemaPath = relResourcePath("schema.csvs")
  val metadataPath = relResourcePath("metaData.csv")
  val warningSchemaPath = relResourcePath("warning.csvs")
  val warningMetadataPath = relResourcePath("warning.csv")
  val badSchemaPath = relResourcePath("badSchema.csvs")
  val standardRulesFailPath = relResourcePath("acceptance/standardRulesFailMetaData.csv")
  val standardRulesSchemaPath = relResourcePath("acceptance/standardRulesSchema.csvs")
  val nonUtf8File = relResourcePath("windows-1252.csv")

  "Check arguments" should {
    "give usage message when no arguments supplied" in {
      CsvValidatorCmdApp.run(Array.empty) must beLike {
        case (errMsg,errCode) => errCode mustEqual SystemExitCodes.IncorrectArguments
      }
    }

    "give usage message when one argument supplied" in {
      CsvValidatorCmdApp.run(Array("meta file")) must beLike {
        case (errMsg,errCode) => errCode mustEqual SystemExitCodes.IncorrectArguments
      }
    }

    "give usage message when too many arguments supplied" in {
      CsvValidatorCmdApp.run(List("somMetaData.csv", "someSchema.csvs", "something extra").toArray) must beLike {
        case (errMsg,errCode) => errCode mustEqual SystemExitCodes.IncorrectArguments
      }
    }

    "fail if metadata file is unreadable" in {
      CsvValidatorCmdApp.run(Array("nonExistentMetaData.csv", schemaPath)) must beLike {
        case (errMsg,errCode) => errCode mustEqual SystemExitCodes.IncorrectArguments
      }
    }

    "fail if schema file is unreadable" in {
      CsvValidatorCmdApp.run(Array(metadataPath, "nonExistentSchema.csvs")) must beLike {
        case (errMsg,errCode) => errCode mustEqual SystemExitCodes.IncorrectArguments
      }
    }

    "fail if both metadata and schema file are unreadable" in {
      CsvValidatorCmdApp.run(Array("nonExistentmetaData.csv", "nonExistentSchema.csvs")) must beLike {
        case (errMsg,errCode) => errCode mustEqual SystemExitCodes.IncorrectArguments
      }
    }

    "succeed if both metadata and schema file are readable" in {
      CsvValidatorCmdApp.run(Array(metadataPath, schemaPath)) must beLike {
        case (errMsg,errCode) => errMsg mustEqual "PASS"
      }
    }
  }

  "Fail fast and file args" should {

    "return true and the file names for fail fast" in {
      CsvValidatorCmdApp.run(Array("--fail-fast", "true", metadataPath, schemaPath)) must beLike {
        case (errMsg, errCode) => errCode mustEqual SystemExitCodes.ValidCsv
      }
    }

    "return true and the file names for fail fast short form" in {
      CsvValidatorCmdApp.run(Array("-f", "true", metadataPath, schemaPath)) must beLike {
        case (errMsg, errCode) => errCode mustEqual SystemExitCodes.ValidCsv
      }
    }

    "return false and the file names for no fail fast" in {
      CsvValidatorCmdApp.run(Array("--fail-fast", "false", "someMetaData.csv", "someSchema.csvs")) must beLike {
        case (errMsg, errCode) => errCode mustEqual SystemExitCodes.IncorrectArguments
      }
    }
  }

  "--path and file args" in {
    //    "find " {
    //
    //    }

    //    "handle --path option" in {
    //      val commandLine = List[String](
    //        "--path:c:=",
    //        "--path:file://c:=file://",
    //        "--fail-fast"
    //      )
    //      CsvValidatorCmdApp.findSubstitutionPaths(commandLine) mustEqual Success(List( ("c:", ""), ("file://c:", "file://")) )
    //    }
    pending
  }

  "Command line app" should {

    "have exit code 0 when validation successful" in {
      CsvValidatorCmdApp.run(Array(metadataPath, schemaPath)) mustEqual Tuple2("PASS", SystemExitCodes.ValidCsv)
    }

    "have exit code 0 when validation only reports warnings and is successful" in {
      CsvValidatorCmdApp.run(Array(warningMetadataPath, warningSchemaPath))._2 mustEqual SystemExitCodes.ValidCsv
    }

    "have exit code 0 when validation only reports warnings and is successful (fail-fast) " in {
      CsvValidatorCmdApp.run(Array("--fail-fast", "true", warningMetadataPath, warningSchemaPath))._2 mustEqual SystemExitCodes.ValidCsv
    }

    //    "have exit code 0 when validation --path successful" in {
    //      CsvValidatorCmdApp.run(Array( "--path:c: " + basePath + "metaData.csv", basePath + "schema.csvs"))._2 mustEqual SystemExitCodes.ValidCsv
    //    }

    "have exit code 1 when the command line arguments are wrong" in {
      CsvValidatorCmdApp.run(Array(""))._2 mustEqual SystemExitCodes.IncorrectArguments
    }

    "have exit code 1 when the --path is missing one args " in {
      CsvValidatorCmdApp.run(Array("--path:c:", metadataPath, schemaPath))._2 mustEqual SystemExitCodes.IncorrectArguments
    }

    "have exit code 1 when the --path is missing both args " in {
      CsvValidatorCmdApp.run(Array("--path", metadataPath, schemaPath))._2 mustEqual SystemExitCodes.IncorrectArguments
    }


    "have exit code 2 when the schema is invalid" in {
      CsvValidatorCmdApp.run(Array(metadataPath, badSchemaPath))._2 mustEqual SystemExitCodes.InvalidSchema
    }

    "have exit code 3 when the metadata is invalid" in {
      CsvValidatorCmdApp.run(Array(standardRulesFailPath, standardRulesSchemaPath))._2 mustEqual SystemExitCodes.InvalidCsv
    }

    "have exit code 0 for non UTF-8 csv file and disabled UTF-8 validation" in {
      CsvValidatorCmdApp.run(Array("--disable-utf8-validation", nonUtf8File, schemaPath)) mustEqual Tuple2("PASS", SystemExitCodes.ValidCsv)
    }

    "have exit code 3 for non UTF-8 csv file" in {
      CsvValidatorCmdApp.run(Array(nonUtf8File, schemaPath))._2 mustEqual SystemExitCodes.InvalidCsv
    }

  }
}