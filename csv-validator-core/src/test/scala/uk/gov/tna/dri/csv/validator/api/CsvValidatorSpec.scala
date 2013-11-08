/**
 * Copyright (c) 2013, The National Archives <digitalpreservation@nationalarchives.gov.uk>
 * http://www.nationalarchives.gov.uk
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package uk.gov.tna.dri.csv.validator.api

import org.specs2.mutable.Specification
import scalaz._
import uk.gov.tna.dri.csv.validator.EOL
import uk.gov.tna.dri.csv.validator.schema.Schema
import java.io.StringReader
import uk.gov.tna.dri.csv.validator.{SchemaMessage, AllErrorsMetaDataValidator}
import scalax.file.Path

class CsvValidatorSpec extends Specification {

  val basePath = "src/test/resources/uk/gov/tna/dri/validator/"

  "Parsing uk.gov.tna.dri.csv.validator.schema" should {
    val app = new CsvValidator with AllErrorsMetaDataValidator { val pathSubstitutions = List[(String,String)]() }

    "report position on parse fail" in {

      val schema =
        """version 1.0
          |@totalColumns 1
          |Name: regox("A")
        """.stripMargin

      app.parseAndValidate(new StringReader(schema)) must beLike {
        case Failure(msgs) =>
          msgs.list mustEqual List(SchemaMessage(
          "[3.7] failure: Invalid uk.gov.tna.dri.csv.validator.schema text" + EOL
          + EOL
          + """Name: regox("A")""" + EOL
          + EOL
          + "      ^"))
      }
    }
  }

  "Validation" should {
    val app = new CsvValidator with AllErrorsMetaDataValidator { val pathSubstitutions = List[(String,String)]() }

    def parse(filePath: String): Schema = app.parseSchema(Path.fromString(filePath)) fold (f => throw new IllegalArgumentException(f.toString()), s => s)

    "succeed for valid uk.gov.tna.dri.csv.validator.schema and metadata file" in {
      app.validate(Path.fromString(basePath) / "metaData.csv", parse(basePath + "uk.gov.tna.dri.csv.validator.schema.txt")) must beLike {
        case Success(_) => ok
      }
    }

    "succeed for valid @totalColumns in uk.gov.tna.dri.csv.validator.schema and metadata file" in {
      app.validate(Path.fromString(basePath) / "metaData.csv", parse(basePath + "uk.gov.tna.dri.csv.validator.schema.txt")) must beLike {
        case Success(_) => ok
      }
    }
  }
}

