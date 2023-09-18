/*
 * Copyright (c) 2013, The National Archives <digitalpreservation@nationalarchives.gov.uk>
 * https://www.nationalarchives.gov.uk
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package uk.gov.nationalarchives.csv.validator.schema.v1_2

import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import uk.gov.nationalarchives.csv.validator.metadata.{Cell, Row}
import uk.gov.nationalarchives.csv.validator.schema.{TotalColumns, Literal, SchemaSpecBase}
import uk.gov.nationalarchives.csv.validator.schema.v1_1.NoExt

/**
 * Created by rhubner on 4/26/16.
 */
@RunWith(classOf[JUnitRunner])
class SchemaSpec extends SchemaSpecBase {

  "UriDecode Arg provider" should {

    "decode url parameter" in {

      val result = UriDecode(Literal(Some("text%20text")), None).referenceValue(1, Row(List(Cell("Germany")), 1), buildSchema1_2(TotalColumns(0))())

      result must beSome("text text")

    }

    "decode URL parameter with different charset" in {
      val result = UriDecode(Literal(Some("text%9Atext")), Some(Literal(Some("windows-1252")))).referenceValue(1, Row(List(Cell("Germany")), 1), buildSchema1_2(TotalColumns(0))())

      result must beSome("text\u0161text")
    }

  }

}
