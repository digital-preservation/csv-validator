/**
 * Copyright (c) 2013, The National Archives <digitalpreservation@nationalarchives.gov.uk>
 * http://www.nationalarchives.gov.uk
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package uk.gov.nationalarchives.csv.validator.api

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import uk.gov.nationalarchives.csv.validator.TestResources
import org.specs2.mutable.Specification
import scalaz._
import uk.gov.nationalarchives.csv.validator.{TestResources, EOL, AllErrorsMetaDataValidator}
import uk.gov.nationalarchives.csv.validator.schema.Schema
import scalax.file.Path
import uk.gov.nationalarchives.csv.validator.api.CsvValidator.SubstitutePath

/**
 * Created by rhubner on 11/12/15.
 */
@RunWith(classOf[JUnitRunner])
class CsvValidatorFileEncodingSpec extends Specification with TestResources {

  "Validation" should {

    val app = new CsvValidator with AllErrorsMetaDataValidator { val pathSubstitutions = List[(String,String)](); val enforceCaseSensitivePathChecks = false; val trace = false }
    def parse(filePath: String): Schema = app.parseSchema(TextFile(Path.fromString(filePath))) fold (f => throw new IllegalArgumentException(f.toString()), s => s)

    "fail for non UTF-8 file" in {
      app.validate(TextFile(Path.fromString(baseResourcePkgPath) / "windows-1252.csv"), parse(baseResourcePkgPath + "/schema.csvs"), None) must beLike {
        case Failure(_) => ok
      }
    }


    "not fail for valid UTF-8 file" in {
      app.validate(TextFile(Path.fromString(baseResourcePkgPath) / "metaData.csv"), parse(baseResourcePkgPath + "/schema.csvs"), None) must beLike {
        case Success(_) => ok
      }
    }

  }


}
