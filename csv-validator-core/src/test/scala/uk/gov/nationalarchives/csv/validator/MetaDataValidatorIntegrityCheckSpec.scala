/**
 * Copyright (c) 2013, The National Archives <digitalpreservation@nationalarchives.gov.uk>
 * http://www.nationalarchives.gov.uk
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package uk.gov.nationalarchives.csv.validator

import java.lang.management.MemoryUsage
import java.util.Date

import org.specs2.mutable.{BeforeAfter, Specification}
import org.specs2.specification.{BeforeAfterExample, BeforeAfterEach}
import uk.gov.nationalarchives.csv.validator.api.{TextFile, CsvValidator}
import uk.gov.nationalarchives.csv.validator.schema.Schema

import scalax.file.Path
import scalaz.{Failure, Success}


class MetaDataValidatorIntegrityCheckSpec extends Specification with TestResources {

  //TODO find a more generic way to do so
  val base = s"${System.getProperty("user.dir")}/csv-validator-core/src/test/resources/uk/gov/nationalarchives/csv/validator/integrityCheck/"

  
  def buildValidator(substitutionPath: List[(String,String)], filenameColumn: Option[String], include: Boolean = false) : CsvValidator = new CsvValidator with AllErrorsMetaDataValidator {
    val pathSubstitutions = substitutionPath
    val enforceCaseSensitivePathChecks = false
  }
  //TODO Memory check
  def getMemorySize(): Map[String, MemoryUsage] = {
    import java.lang.management.ManagementFactory;
    import java.lang.management.MemoryType;
    import scala.collection.JavaConversions._

    (for {
      mpBean <- ManagementFactory.getMemoryPoolMXBeans() if (mpBean.getType() == MemoryType.HEAP)
    }yield (mpBean.getName(), mpBean.getUsage())).toList.toMap 
    
  }


  def parse(filePath: String, validator: CsvValidator): Schema = validator.parseSchema(TextFile(Path.fromString(filePath))) fold (f => throw new IllegalArgumentException(f.toString()), s => s)

  "integrity Check" should {
    val headerPath = base + "/header/"
    val noHeaderPath = base + "/noheader/"
    val WO95Path = base + "/WO_95/"
    
    "succeed with good values - header" in {

      val substititutionPaths = List(("file:///T:/WORK/RF_5/",headerPath))
      val validator = buildValidator(substititutionPaths, Some("filename"))
      validator.validate(TextFile(Path.fromString(headerPath) / "integrityCheckMetaData.csv"), parse(headerPath + "/integrityCheckSchema.csvs",validator), None) must beLike {
         case Success(_) => ok
      }

    }

    "succeed with good values and implicit include folder - header" in {

      val substititutionPaths = List(("file:///T:/WORK/RF_5/",headerPath))
      val validator = buildValidator(substititutionPaths, Some("filename"))
      validator.validate(TextFile(Path.fromString(headerPath) / "integrityCheckMetaData.csv"), parse(headerPath + "/integrityCheckDefaultIncludeFolderSchema.csvs",validator), None) must beLike {
        case Success(_) => ok
      }

    }


    "fail for metadatafile missing files - header" in {

      val substititutionPaths = List(("file:///T:/WORK/RF_5/",headerPath))
      val validator = buildValidator(substititutionPaths, Some("filename"))
      val result = validator.validate(TextFile(Path.fromString(headerPath) / "integrityCheckMetaData-missing-files.csv"), parse(headerPath + "/integrityCheckSchema.csvs",validator), None)

      result.isFailure mustEqual true
      val Failure(message) = result
      //TODO perform test on nonEmptyList instead of using to string
      message.toString  must contain("[Integrity Check]")
      message.toString  must contain("file2 are not listed in ")
    }

    "fail for metadatafile missing files - header" in {

      val substititutionPaths = List(("file:///T:/WORK/RF_5/",headerPath))
      val validator = buildValidator(substititutionPaths, Some("filename"))
      val result = validator.validate(TextFile(Path.fromString(headerPath) / "integrityCheckMetaData-missing-files.csv"), parse(headerPath + "/integrityCheckSchema.csvs",validator), None)

      result.isFailure mustEqual true
      val Failure(message) = result
      //TODO perform test on nonEmptyList instead of using to string
      message.toString  must contain("[Integrity Check]")
      message.toString  must contain("file2 are not listed in ")
    }


    "succeed with good values - no header" in {

      val substititutionPaths = List(("file:///T:/WORK/RF_5/",noHeaderPath))
      val validator = buildValidator(substititutionPaths, Some("identifier"))
      val schema: Schema = parse(noHeaderPath + "/integrityCheckSchema.csvs", validator)
      validator.validate(TextFile(Path.fromString(noHeaderPath) / "integrityCheckMetaData.csv"), schema, None) must beLike {
        case Success(_) => ok
      }

    }

  //FIXME
//    "fail with wrong includeFolder directive - no header" in {
//
//      val substititutionPaths = List(("file:///T:/WORK/RF_5/",noHeaderPath))
//      val validator = buildValidator(substititutionPaths, Some("filename"))
//      val result = validator.validate(TextFile(Path.fromString(noHeaderPath) / "integrityCheckMetaData.csv"), parse(noHeaderPath + "/badIntegrityCheckSchema.csvs",validator), None)
//      result.isFailure mustEqual true
//      val Failure(message) = result
//      println(message)
//      ok
//    }

    "Validate WO 95" in {

      val substititutionPaths = List(("file:///WO_95",WO95Path))
      val validator = buildValidator(substititutionPaths, Some("file_path"), false)
      validator.validate(TextFile(Path.fromString(WO95Path) / "tech_acq_metadata_v1_WO95Y14B003.csv"), parse(WO95Path + "/tech_acq_metadata_v1_WO95Y14B000.csvs",validator), None) must beLike {
        case Success(_) => ok
      }
    }

  }

}
