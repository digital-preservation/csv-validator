/**
 * Copyright (c) 2013, The National Archives <digitalpreservation@nationalarchives.gov.uk>
 * http://www.nationalarchives.gov.uk
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
import scalaz._
import uk.gov.nationalarchives.csv.validator._
import uk.gov.nationalarchives.csv.validator.schema.Schema
import scalax.file.Path
import uk.gov.nationalarchives.csv.validator.api.CsvValidator.SubstitutePath

@RunWith(classOf[JUnitRunner])
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
        case Failure(msgs) =>
          msgs.list mustEqual IList(FailMessage(SchemaDefinitionError,
          "[3.7] failure: Invalid column definition" + EOL
          + EOL
          + """Name: regox("A")""" + EOL
          + "      ^"))
      }
    }
  }

  "Validation" should {
    val app = new CsvValidator with AllErrorsMetaDataValidator { val pathSubstitutions = List[(String,String)](); val enforceCaseSensitivePathChecks = false; val trace = false }

    def parse(filePath: String): Schema = app.parseSchema(TextFile(Path.fromString(filePath))) fold (f => throw new IllegalArgumentException(f.toString()), s => s)

    "succeed for valid schema and metadata file" in {
      app.validate(TextFile(Path.fromString(baseResourcePkgPath) / "metaData.csv"), parse(baseResourcePkgPath + "/schema.csvs"), None) must beLike {
        case Success(_) => ok
      }
    }

    "succeed for valid @totalColumns in schema and metadata file" in {
      app.validate(TextFile(Path.fromString(baseResourcePkgPath) / "metaData.csv"), parse(baseResourcePkgPath + "/schema.csvs"), None) must beLike {
        case Success(_) => ok
      }
    }
  }
}

