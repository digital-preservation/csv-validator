/**
 * Copyright (c) 2013, The National Archives <digitalpreservation@nationalarchives.gov.uk>
 * http://www.nationalarchives.gov.uk
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package uk.gov.nationalarchives.csv.validator.cmd

import scala.Array
import org.specs2.mutable.Specification

class CsvValidatorCmdAppSpec extends Specification with TestResources {

  val schemaPath = relResourcePath("schema.csvs")
  val metadataPath = relResourcePath("metaData.csv")
  val badSchemaPath = relResourcePath("badSchema.csvs")
  val standardRulesFailPath = relResourcePath("acceptance/standardRulesFailMetaData.csv")
  val standardRulesSchemaPath = relResourcePath("acceptance/standardRulesSchema.csvs")
  val integrityCheckSchemaPath = relResourcePath("integrityCheck/header/schema.csvs")
  val integrityCheckMetaDataPath = relResourcePath("integrityCheck/header/metaData.csv")
  val integrityCheckMetaDataPathMissingFiles = relResourcePath("integrityCheck/header/metaData-missing-files.csv")
  val integrityCheckMetaDataPathWithTooManyFiles = relResourcePath("integrityCheck/header/metaData-with-too-many-files.csv")
  val integrityCheckBadSchemaPath = relResourcePath("integrityCheck/header/badSchema.csvs")
  val integrityCheckSchemaPathNoHeader = relResourcePath("integrityCheck/noheader/schema.csvs")
  val integrityCheckMetaDataPathNoHeader = relResourcePath("integrityCheck/noheader/metaData.csv")


  "Check arguments" should {
    "give usage message when no arguments supplied" in {
      CsvValidatorCmdApp.run(Array.empty) must beLike {
        case (errMsg,errCode) => errCode mustEqual SystemExits.IncorrectArguments
      }
    }

    "give usage message when one argument supplied" in {
      CsvValidatorCmdApp.run(Array("meta file")) must beLike {
        case (errMsg,errCode) => errCode mustEqual SystemExits.IncorrectArguments
      }
    }

    //    "give usage message when too many arguments supplied" in {
    //      CsvValidatorCmdApp.run(List("somMetaData.csv", "someSchema.csvs", "something extra").toArray) must beLike {
    //        case (errMsg,errCode) => errCode mustEqual SystemExits.IncorrectArguments
    //      }
    //    }

    "fail if metadata file is unreadable" in {
      CsvValidatorCmdApp.run(Array("nonExistentMetaData.csv", schemaPath)) must beLike {
        case (errMsg,errCode) => errCode mustEqual SystemExits.IncorrectArguments
      }
    }

    "fail if schema file is unreadable" in {
      CsvValidatorCmdApp.run(Array(metadataPath, "nonExistentSchema.csvs")) must beLike {
        case (errMsg,errCode) => errCode mustEqual SystemExits.IncorrectArguments
      }
    }

    "fail if both metadata and schema file are unreadable" in {
      CsvValidatorCmdApp.run(Array("nonExistentmetaData.csv", "nonExistentSchema.csvs")) must beLike {
        case (errMsg,errCode) => errCode mustEqual SystemExits.IncorrectArguments
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
      CsvValidatorCmdApp.run(Array("--fail-fast", "someMetaData.csv", "someSchema.csvs")) must beLike {
        case (errMsg, errCode) => errCode mustEqual SystemExits.IncorrectArguments
      }
    }

    "return true and the file names for fail fast short form" in {
      CsvValidatorCmdApp.run(Array("-f", "someMetaData.csv", "someSchema.csvs")) must beLike {
        case (errMsg, errCode) => errCode mustEqual SystemExits.IncorrectArguments
      }
    }

    "return false and the file names for no fail fast" in {
      CsvValidatorCmdApp.run(Array("someMetaData.csv", "someSchema.csvs")) must beLike {
        case (errMsg, errCode) => errCode mustEqual SystemExits.IncorrectArguments
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
      CsvValidatorCmdApp.run(Array(metadataPath, schemaPath)) mustEqual Tuple2("PASS", 0)
    }

    //    "have exit code 0 when validation --path successful" in {
    //      CsvValidatorCmdApp.run(Array( "--path:c: " + basePath + "metaData.csv", basePath + "schema.csvs"))._2 mustEqual 0
    //    }
    
    "have exit code 1 when the command line arguments are wrong" in {
      CsvValidatorCmdApp.run(Array(""))._2 mustEqual 1
    }

    "have exit code 1 when the --path is missing one args " in {
      CsvValidatorCmdApp.run(Array("--path:c:", metadataPath, schemaPath))._2 mustEqual 1
    }

    "have exit code 1 when the --path is missing both args " in {
      CsvValidatorCmdApp.run(Array("--path", metadataPath, schemaPath))._2 mustEqual 1
    }


    "have exit code 2 when the schema is invalid" in {
      CsvValidatorCmdApp.run(Array(metadataPath, badSchemaPath))._2 mustEqual 2
    }

    "have exit code 3 when the metadata is invalid" in {
      CsvValidatorCmdApp.run(Array(standardRulesFailPath, standardRulesSchemaPath))._2 mustEqual 3
    }


    "have exit code 1 when integrity check fails because it can't find the content folder" in {
      CsvValidatorCmdApp.run(Array("-i","Name", metadataPath, schemaPath))._2  === 3
    }



    "have exit code 0 when validation successful with integrity check - for schema with no header" in {
      CsvValidatorCmdApp.run(Array("-i","Name", integrityCheckMetaDataPathNoHeader, integrityCheckSchemaPathNoHeader)) mustEqual Tuple2("PASS", 0)
    }

    "have exit code 0 when the metadata file list has more files than the actual content file" in {
      val result = CsvValidatorCmdApp.run(Array("-i","Name", integrityCheckMetaDataPathWithTooManyFiles, integrityCheckSchemaPath))

      result mustEqual Tuple2("PASS", 0)

    }

    "have exit code 3 when the metadata file list fewer wile than the actual content file with integrity check error message" in {
      val result = CsvValidatorCmdApp.run(Array("-i","Name", integrityCheckMetaDataPathMissingFiles, integrityCheckSchemaPath))
      result._2 mustEqual 3
      result._1 must contain("[Integrity Check]")
    }


    "have exit code 0 when validation successful with integrity check" in {
       CsvValidatorCmdApp.run(Array("-i","Name", integrityCheckMetaDataPath, integrityCheckSchemaPath)) mustEqual Tuple2("PASS", 0)
    }






    //TODO Add some more integrity check tests...

  }
}