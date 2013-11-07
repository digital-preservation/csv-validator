/*
 * Copyright (c) 2013, The National Archives digitalpreservation@nationalarchives.gov.uk
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

class MetaDataValidatorBigFileSpec extends Specification {

  val basePath = "src/test/resources/uk/gov/tna/dri/validator/acceptance/"

  "Big file" should {

    "succeed with no stack overflow for all errors" in {
      val v = new CsvValidator with AllErrorsMetaDataValidator { val pathSubstitutions = List[(String,String)]() }
      def parse(filePath: String): Schema = v.parseSchema(Path.fromString(filePath)) fold (f => throw new IllegalArgumentException(f.toString()), s => s)

      v.validate(Path.fromString(basePath) / "bigMetaData.csv", parse(basePath + "bigSchema.txt")) must beLike { case Success(_) => ok }
    }

    "succeed with no stack overflow for fail fast" in {
      val v = new CsvValidator with FailFastMetaDataValidator { val pathSubstitutions = List[(String,String)]() }
      def parse(filePath: String): Schema = v.parseSchema(Path.fromString(filePath)) fold (f => throw new IllegalArgumentException(f.toString()), s => s)

      v.validate(Path.fromString(basePath) / "bigMetaData.csv", parse(basePath + "bigSchema.txt")) must beLike { case Success(_) => ok }
    }
  }
}