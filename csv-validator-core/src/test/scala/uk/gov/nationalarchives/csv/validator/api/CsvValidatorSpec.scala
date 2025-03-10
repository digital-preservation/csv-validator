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
import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import cats.data.Validated
import uk.gov.nationalarchives.csv.validator._
import uk.gov.nationalarchives.csv.validator.schema.Schema
import uk.gov.nationalarchives.csv.validator.api.CsvValidator.SubstitutePath

import java.nio.file.Paths

@RunWith(classOf[JUnitRunner])
class CsvValidatorSpec extends Specification with TestResources {

  "Parsing schema" should {
    val app = new CsvValidator with AllErrorsMetaDataValidator { val pathSubstitutions = List[SubstitutePath](); val enforceCaseSensitivePathChecks = false; val trace = false; val skipFileChecks = false; val maxCharsPerCell = 4096 }

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
    val app = new CsvValidator with AllErrorsMetaDataValidator { val pathSubstitutions = List[(String,String)](); val enforceCaseSensitivePathChecks = false; val trace = false; val skipFileChecks = false; val maxCharsPerCell = 4096 }

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

    val callback = new ProgressCallback {
      var processed = -1
      var total = -2
      override def update(complete: Percentage): Unit = throw new NotImplementedError()

      override def update(_total: Int, _processed: Int): Unit = {
        total = _total
        processed = _processed
      }
    }

    "have a total (of rows) equal to the actual number of rows in the metadata file even if there are multiple line breaks in a cell" in {
      app.validate(TextFile(Paths.get(baseResourcePkgPath).resolve("metadataMultipleLineBreaksInCell.csv")), parse(baseResourcePkgPath + "/schema.csvs"), Some(callback)) must beLike {
        case Validated.Valid(_) => ok
      }
      callback.total must beEqualTo(2)
    }
  }
}

