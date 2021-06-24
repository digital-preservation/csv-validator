/**
 * Copyright (c) 2013, The National Archives <digitalpreservation@nationalarchives.gov.uk>
 * https://www.nationalarchives.gov.uk
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package uk.gov.nationalarchives.csv.validator.schema.v1_1

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import uk.gov.nationalarchives.csv.validator.metadata.{Cell, Row}
import uk.gov.nationalarchives.csv.validator.schema.{TotalColumns, SchemaSpecBase, Literal, Schema}

@RunWith(classOf[JUnitRunner])
class SchemaSpec extends SchemaSpecBase {

  "NoExt Arg provider" should {

    "remove extension from string" in {

      val result = NoExt(Literal(Some("somestringwithextension.txt"))).referenceValue(1, Row(List(Cell("Germany")), 1), buildSchema1_1(TotalColumns(0))())

      result must_=== Some("somestringwithextension")
    }

    "leave a string without extension unchanged" in {

      val result = NoExt(Literal(Some("somestringwithoutextension"))).referenceValue(1, Row(List(Cell("Germany")), 1), buildSchema1_1(TotalColumns(0))())

      result must_=== Some("somestringwithoutextension")
    }

  }

  "Concat Arg provider" should {

    " append literal together" in {

      val result1 = Concat(Literal(Some("aaaaa")), Literal(None)).referenceValue(1, Row(List(Cell("Germany")), 1), buildSchema1_1(TotalColumns(0))())

      result1 must_=== Some("aaaaa")

      val result2 = Concat(Literal(None), Literal(Some("aaaaa"))).referenceValue(1, Row(List(Cell("Germany")), 1), buildSchema1_1(TotalColumns(0))())

      result2 must_=== Some("aaaaa")

      val result3 = Concat(Literal(Some("aaaaa")), Literal(Some("bbbbb"))).referenceValue(1, Row(List(Cell("Germany")), 1), buildSchema1_1(TotalColumns(0))())

      result3 must_=== Some("aaaaabbbbb")

      val result4 = Concat(Literal(None), Literal(None)).referenceValue(1, Row(List(Cell("Germany")), 1), buildSchema1_1(TotalColumns(0))())

      result4 must_=== None
    }
  }

}
