/**
 * Copyright (c) 2013, The National Archives <digitalpreservation@nationalarchives.gov.uk>
 * http://www.nationalarchives.gov.uk
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package uk.gov.nationalarchives.csv.validator

import java.io.File
import java.lang.management.MemoryUsage

import org.specs2.mutable.Specification
import uk.gov.nationalarchives.csv.validator.api.{CsvValidator, TextFile}
import uk.gov.nationalarchives.csv.validator.schema.{TraceableParsers, Schema}

import scalax.file.Path
import scalaz.{Failure, Success}


class MetaDataValidatorIntegrityCheckSpec extends Specification with TestResources {

  def buildValidator(substitutionPath: List[(String,String)]) : CsvValidator = new CsvValidator with AllErrorsMetaDataValidator {
    val pathSubstitutions = substitutionPath
    val enforceCaseSensitivePathChecks = false
    val trace = false
  }

  def parse(filePath: String, validator: CsvValidator): Schema = validator.parseSchema(TextFile(Path.fromString(filePath))) fold (f => throw new IllegalArgumentException(f.toString()), s => s)

  "integrity Check" should {
    val headerPath = integrityCheckPath + "/header/"
    val noHeaderPath = integrityCheckPath + "/noheader/"
    val WO95Path = integrityCheckPath + "/WO_95/"

    "succeed with good values - header" in {

      val substitutionPaths = List(("file:///T:/WORK/RF_5/",headerPath))
      val validator = buildValidator(substitutionPaths)
      validator.validate(TextFile(Path.fromString(headerPath) / "integrityCheckMetaData.csv"), parse(headerPath + "/integrityCheckSchema.csvs",validator), None).isSuccess mustEqual true


    }

    "succeed with good values and exclude folder paramter - header" in {

      val substitutionPaths = List(("file:///T:/WORK/RF_5/",headerPath))
      val validator = buildValidator(substitutionPaths)
      validator.validate(TextFile(Path.fromString(headerPath) / "integrityCheckMetaData.csv"), parse(headerPath + "/integrityCheckDefaultIncludeFolderSchema.csvs",validator), None).isSuccess mustEqual true

    }


    "fail for metadatafile missing files - header" in {

      val substitutionPaths = List(("file:///T:/WORK/RF_5/",headerPath))
      //      val validator = buildValidator(substitutionPaths, Some("filename"))
      val validator = buildValidator(substitutionPaths)
      val result = validator.validate(TextFile(Path.fromString(headerPath) / "integrityCheckMetaData-missing-files.csv"), parse(headerPath + "/integrityCheckSchema.csvs",validator), None)

      result.isFailure mustEqual true
      val Failure(message) = result
      //TODO perform test on nonEmptyList instead of using to string
      message.toString  must contain("integrityCheck fails for")
      //      message.toString  must contain("file2 are not listed in ")
    }

    "fail for metadatafile missing files - header" in {

      val substitutionPaths = List(("file:///T:/WORK/RF_5/",headerPath))
      val validator = buildValidator(substitutionPaths)
      val result = validator.validate(TextFile(Path.fromString(headerPath) / "integrityCheckMetaData-missing-files.csv"), parse(headerPath + "/integrityCheckSchema.csvs",validator), None)

      result.isFailure mustEqual true
      val Failure(message) = result
      //TODO perform test on nonEmptyList instead of using to string
      message.toString  must contain("integrityCheck fails for")
      //          message.toString  must contain("file2 are not listed in ")
    }


    "succeed with good values - no header" in {

      val substitutionPaths = List(("file:///T:/WORK/RF_5/",noHeaderPath))
      val validator = buildValidator(substitutionPaths)
      val schema: Schema = parse(noHeaderPath + "/integrityCheckSchema.csvs", validator)
      validator.validate(TextFile(Path.fromString(noHeaderPath) / "integrityCheckMetaData.csv"), schema, None).isSuccess mustEqual true

    }


    "fail with wrong includeFolder directive - no header" in {

      val substitutionPaths = List(("file:///T:/WORK/RF_5/",noHeaderPath))
      val validator = buildValidator(substitutionPaths)
      val result = validator.validate(TextFile(Path.fromString(noHeaderPath) / "integrityCheckMetaData.csv"), parse(noHeaderPath + "/badIntegrityCheckSchema.csvs",validator), None)
      result.isFailure mustEqual true
      val Failure(message) = result
      // println(message)
      ok
    }.pendingUntilFixed()

    "Validate WO 95" in {

      val substitutionPaths = List(("file:///WO_95",WO95Path))
      val validator = buildValidator(substitutionPaths)
      validator.validate(TextFile(Path.fromString(WO95Path) / "tech_acq_metadata_v1_WO95Y14B003.csv"), parse(WO95Path + "/tech_acq_metadata_v1_WO95Y14B000.csvs",validator), None).isSuccess mustEqual true
    }

  }

}