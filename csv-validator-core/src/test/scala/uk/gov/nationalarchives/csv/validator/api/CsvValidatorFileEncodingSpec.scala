/*
 * Copyright (c) 2013, The National Archives <digitalpreservation@nationalarchives.gov.uk>
 * https://www.nationalarchives.gov.uk
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package uk.gov.nationalarchives.csv.validator.api

import org.specs2.mutable.Specification
import uk.gov.nationalarchives.csv.validator.TestResources
import cats.data.Validated
import uk.gov.nationalarchives.csv.validator.AllErrorsMetaDataValidator
import uk.gov.nationalarchives.csv.validator.schema.Schema

import java.nio.file.Paths

/**
 * Created by rhubner on 11/12/15.
 */
class CsvValidatorFileEncodingSpec extends Specification with TestResources {

  "Validation" should {

    val app = new CsvValidator with AllErrorsMetaDataValidator { val pathSubstitutions = List[(String,String)](); val enforceCaseSensitivePathChecks = false; val trace = false }
    def parse(filePath: String): Schema = app.parseSchema(TextFile(Paths.get(filePath))) fold (f => throw new IllegalArgumentException(f.toString()), s => s)

    "fail for non UTF-8 file" in {
      app.validate(TextFile(Paths.get(baseResourcePkgPath).resolve("windows-1252.csv")), parse(baseResourcePkgPath + "/schema.csvs"), None) must beLike {
        case Validated.Invalid(_) => ok
      }
    }


    "not fail for valid UTF-8 file" in {
      app.validate(TextFile(Paths.get(baseResourcePkgPath).resolve("metaData.csv")), parse(baseResourcePkgPath + "/schema.csvs"), None) must beLike {
        case Validated.Valid(_) => ok
      }
    }

  }


}
