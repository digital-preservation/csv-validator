/**
 * Copyright (c) 2013, The National Archives <digitalpreservation@nationalarchives.gov.uk>
 * http://www.nationalarchives.gov.uk
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package uk.gov.nationalarchives.csv.validator.schema.v1_0

import java.io.StringReader

import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import uk.gov.nationalarchives.csv.validator.schema._

@RunWith(classOf[JUnitRunner])
class SchemaParserTotalColumnsSpec extends SchemaSpecBase {

  import TestSchemaParser._

  "Schema" should {

    "fail for TotalColumns with missing value" in {
      parse(new StringReader("version 1.0\n@totalColumns")) must beLike { case Failure(message, _) => message mustEqual "Invalid global directive" }
    }

    "fail for incorrect TotalColumns field name" in {
      parse(new StringReader("version 1.0\n@ToalColumns 23")) must beLike { case Failure(message, _) => message mustEqual "Invalid global directive" }
    }

    "fail for incorrect TotalColumns field name with no value" in {
      parse(new StringReader("version 1.0\n@TtalColumn")) must beLike { case Failure(message, _) => message mustEqual "Invalid global directive" }
    }

    "fail for TotalColumns field name incorrect case" in {
      parse(new StringReader("version 1.0\n@TotalColumns 65")) must beLike { case Failure(message, _) => message mustEqual "Invalid global directive" }
    }

    "fail for TotalColumns of zero" in {
      parse(new StringReader("version 1.0\n@totalColumns 0")) must beLike { case Failure(message, _) => message mustEqual "Invalid global directive" }
    }

    "fail for TotalColumns with negative integer" in {
      parse(new StringReader("version 1.0\n@totalColumns -23")) must beLike { case Failure(message, _) => message mustEqual "Invalid global directive" }
    }

    "fail for TotalColumns with non integer" in {
      parse(new StringReader("version 1.0\n@totalColumns 132.45")) must beLike { case Failure(message, _) => message mustEqual "Invalid column definition" }
    }

    "fail for TotalColumns with non numeric" in {
      parse(new StringReader("version 1.0\n@totalColumns blah")) must beLike { case Failure(message, _) => message mustEqual "Invalid global directive" }
    }
  }
}