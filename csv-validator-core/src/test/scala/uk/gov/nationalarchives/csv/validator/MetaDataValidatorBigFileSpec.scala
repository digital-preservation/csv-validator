/*
 * Copyright (c) 2013, The National Archives <digitalpreservation@nationalarchives.gov.uk>
 * https://www.nationalarchives.gov.uk
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package uk.gov.nationalarchives.csv.validator

import org.specs2.mutable.Specification
import uk.gov.nationalarchives.csv.validator.schema.Schema
import uk.gov.nationalarchives.csv.validator.api.{CsvValidator, TextFile}
import uk.gov.nationalarchives.csv.validator.api.CsvValidator.SubstitutePath
import cats.data.Validated
import java.nio.file.Paths

class MetaDataValidatorBigFileSpec extends Specification with TestResources {

  val base = acceptancePath

  "Big file" should {

    "succeed with no stack overflow for all errors" in {
      val v = new CsvValidator with AllErrorsMetaDataValidator { val pathSubstitutions = List[SubstitutePath](); val enforceCaseSensitivePathChecks = false; val trace = false }
      def parse(filePath: String): Schema = v.parseSchema(TextFile(Paths.get(filePath))) fold (f => throw new IllegalArgumentException(f.toString()), s => s)

      v.validate(TextFile(Paths.get(base).resolve("bigMetaData.csv")), parse(base + "/bigSchema.csvs"), None) must beLike { case Validated.Valid(_) => ok }
    }

    "succeed with no stack overflow for fail fast" in {
      val v = new CsvValidator with FailFastMetaDataValidator { val pathSubstitutions = List[SubstitutePath](); val enforceCaseSensitivePathChecks = false; val trace = false }
      def parse(filePath: String): Schema = v.parseSchema(TextFile(Paths.get(filePath))) fold (f => throw new IllegalArgumentException(f.toString()), s => s)

      v.validate(TextFile(Paths.get(base).resolve("bigMetaData.csv")), parse(base + "/bigSchema.csvs"), None) must beLike { case Validated.Valid(_) => ok }
    }
  }
}
