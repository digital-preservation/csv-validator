/**
 * Copyright (c) 2013, The National Archives <digitalpreservation@nationalarchives.gov.uk>
 * http://www.nationalarchives.gov.uk
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
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