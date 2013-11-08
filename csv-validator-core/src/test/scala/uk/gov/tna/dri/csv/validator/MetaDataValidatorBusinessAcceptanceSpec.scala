/**
 * Copyright (c) 2013, The National Archives <digitalpreservation@nationalarchives.gov.uk>
 * http://www.nationalarchives.gov.uk
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package uk.gov.tna.dri.csv.validator

import org.specs2.mutable.Specification
import scalaz._
import uk.gov.tna.dri.csv.validator.schema.Schema
import uk.gov.tna.dri.csv.validator.api.CsvValidator
import scalax.file.Path

class MetaDataValidatorBusinessAcceptanceSpec extends Specification {

  val basePath = "src/test/resources/uk/gov/tna/dri/validator/acceptance/dp/"

  val v: CsvValidator = new CsvValidator with AllErrorsMetaDataValidator { val pathSubstitutions = List[(String,String)]() }
  import v.{validate, parseSchema}

  def parse(filePath: String): Schema = parseSchema(Path.fromString(filePath)) fold (f => throw new IllegalArgumentException(f.toString()), s => s)

  "Regex rule" should {

    "succeed" in {
      validate(Path.fromString(basePath) / "regexRulePassMetaData.csv", parse(basePath + "regexRuleSchema.txt")) must beLike {
        case Success(_) => ok
      }
    }

    "fail" in {
      validate(Path.fromString(basePath) / "regexRuleFailMetaData.csv", parse(basePath + "regexRuleSchema.txt")) must beLike {
        case Failure(_) => ok
      }
    }
  }

}