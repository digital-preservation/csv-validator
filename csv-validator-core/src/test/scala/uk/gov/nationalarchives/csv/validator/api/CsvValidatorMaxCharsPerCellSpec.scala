/*
 * Copyright (c) 2013, The National Archives <digitalpreservation@nationalarchives.gov.uk>
 * https://www.nationalarchives.gov.uk
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package uk.gov.nationalarchives.csv.validator.api

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import uk.gov.nationalarchives.csv.validator.schema.Schema
import uk.gov.nationalarchives.csv.validator.{AllErrorsMetaDataValidator, FailMessage, TestResources, ValidationError}

import java.nio.file.Paths


@RunWith(classOf[JUnitRunner])
class CsvValidatorMaxCharsPerCellSpec extends Specification with TestResources {

  "Validation" should {

    def app(maxChars: Int=4096) = new CsvValidator with AllErrorsMetaDataValidator { val pathSubstitutions: List[(String, String)] = List[(String,String)](); val enforceCaseSensitivePathChecks = false; val trace = false; val skipFileChecks = false; val maxCharsPerCell: Int = maxChars }
    def parse(filePath: String, maxChars: Int=4096): Schema = app(maxChars).parseSchema(TextFile(Paths.get(filePath))) fold (f => throw new IllegalArgumentException(f.toString()), s => s)

    "fail if the number of characters in a cell in the header is more than the maxCharsPerCell number and indicate the column number" in {
      val maxCharsAllowed = 2
      val validatedNel = app(maxCharsAllowed).validate(TextFile(Paths.get(baseResourcePkgPath).resolve("metaData.csv")), parse(baseResourcePkgPath + "/schema.csvs", maxCharsAllowed), None).swap
      validatedNel.toList.head.toList must beEqualTo(
        List(FailMessage(ValidationError,"java.lang.Exception: The number of characters in column 1 of the header row is larger than the maximum number of characters allowed in a cell (2); increase this limit and re-run.",None,None))
      )
    }

    "fail if the number of characters in a cell in a non-header row is more than the maxCharsPerCell number and indicate the column name" in {
      val maxCharsAllowed = 15
      val validatedNel = app(maxCharsAllowed).validate(TextFile(Paths.get(baseResourcePkgPath).resolve("metaDataWithALongCellLength.csv")), parse(baseResourcePkgPath + "/schema.csvs"), None).swap
      validatedNel.toList.head.toList must beEqualTo(
        List(FailMessage(ValidationError,"java.lang.Exception: The number of characters in the cell located at row: 1, column: col2, is larger than the maximum number of characters allowed in a cell (15); increase this limit and re-run.",None,None))
      )
    }

    "not fail if the number of characters in a cell is less than the maxCharsPerCell number" in {
      val validatedNel = app().validate(TextFile(Paths.get(baseResourcePkgPath).resolve("metaData.csv")), parse(baseResourcePkgPath + "/schema.csvs"), None)
      validatedNel.toList must beEqualTo(List(()))
    }
  }
}
