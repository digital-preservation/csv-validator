/*
 * Copyright (c) 2013, The National Archives <digitalpreservation@nationalarchives.gov.uk>
 * https://www.nationalarchives.gov.uk
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package uk.gov.nationalarchives.csv.validator.api

import java.io.StringReader
import org.specs2.mutable.Specification
import cats.data.Validated
import uk.gov.nationalarchives.csv.validator._
import uk.gov.nationalarchives.csv.validator.schema.Schema
import uk.gov.nationalarchives.csv.validator.api.CsvValidator.SubstitutePath

import java.nio.file.Paths

class CsvValidatorSpec extends Specification with TestResources {

  "Parsing schema" should {
    val app = new CsvValidator with AllErrorsMetaDataValidator { val pathSubstitutions = List[SubstitutePath](); val enforceCaseSensitivePathChecks = false; val trace = false }

    "report position on parse fail" in {

      val schema =
        """version 1.0
          |@totalColumns 1
          |Name: regox("A")
        """.stripMargin

      app.parseAndValidate(new StringReader(schema)) must beLike {
        case Validated.Invalid(msgs) =>
          msgs.toList mustEqual List(FailMessage(SchemaDefinitionError,
          "[3.7] failure: Invalid column definition" + EOL
          + EOL
          + """Name: regox("A")""" + EOL
          + "      ^"))
      }
    }
  }

  "Validation" should {
    val app = new CsvValidator with AllErrorsMetaDataValidator { val pathSubstitutions = List[(String,String)](); val enforceCaseSensitivePathChecks = false; val trace = false }

    def parse(filePath: String): Schema = app.parseSchema(TextFile(Paths.get(filePath))) fold (f => throw new IllegalArgumentException(f.toString()), s => s)

    "succeed for valid schema and metadata file" in {
      app.validate(TextFile(Paths.get(baseResourcePkgPath).resolve("metaData.csv")), parse(baseResourcePkgPath + "/schema.csvs"), None) must beLike {
        case Validated.Valid(_) => ok
      }
    }

    "succeed for valid @totalColumns in schema and metadata file" in {
      app.validate(TextFile(Paths.get(baseResourcePkgPath).resolve("metaData.csv")), parse(baseResourcePkgPath + "/schema.csvs"), None) must beLike {
        case Validated.Valid(_) => ok
      }
    }
  }
}

