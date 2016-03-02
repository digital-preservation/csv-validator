/**
 * Copyright (c) 2013, The National Archives <digitalpreservation@nationalarchives.gov.uk>
 * http://www.nationalarchives.gov.uk
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package uk.gov.nationalarchives.csv.validator

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import scalaz._
import uk.gov.nationalarchives.csv.validator.schema.Schema
import uk.gov.nationalarchives.csv.validator.api.{TextFile, CsvValidator}
import scalax.file.Path

@RunWith(classOf[JUnitRunner])
class MetaDataValidatorBusinessAcceptanceSpec extends Specification with TestResources {

  val base = resourcePath("acceptance/dp")

  val v: CsvValidator = new CsvValidator with AllErrorsMetaDataValidator { val pathSubstitutions = List[(String,String)](); val enforceCaseSensitivePathChecks = false; val trace = false }
  import v.{validate, parseSchema}

  def parse(filePath: String): Schema = parseSchema(TextFile(Path.fromString(filePath))) fold (f => throw new IllegalArgumentException(f.toString()), s => s)

  "Regex rule" should {

    "succeed" in {
      validate(TextFile(Path.fromString(base) / "regexRulePassMetaData.csv"), parse(base + "/regexRuleSchema.csvs"), None) must beLike {
        case Success(_) => ok
      }
    }

    "fail" in {
      validate(TextFile(Path.fromString(base) / "regexRuleFailMetaData.csv"), parse(base + "/regexRuleSchema.csvs"), None) must beLike {
        case Failure(_) => ok
      }
    }
  }

}