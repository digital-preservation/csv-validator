/*
 * Copyright (c) 2013, The National Archives <digitalpreservation@nationalarchives.gov.uk>
 * https://www.nationalarchives.gov.uk
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package uk.gov.nationalarchives.csv.validator.schema.v1_0

import cats.data.Validated
import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import uk.gov.nationalarchives.csv.validator.metadata.{Cell, Row}
import uk.gov.nationalarchives.csv.validator.schema._

@RunWith(classOf[JUnitRunner])
class NotRuleSpec extends Specification {

  "NotRule with a string literal behaviour" should {
    val globalDirsOne = List(TotalColumns(1))

    "succeed if notRule is not the same as value" in {
      val notRule = NotRule(Literal(Some("completely different value")))
      notRule.evaluate(0, Row(List(Cell("hello world")), 1), Schema(globalDirsOne, List(ColumnDefinition(NamedColumnIdentifier("column1"))))) mustEqual Validated.Valid(true)
    }

    "succeed if notRule is the similar to value" in {
      val notRule = NotRule(Literal(Some("hello world ")))
      notRule.evaluate(0, Row(List(Cell("hello world")), 1), Schema(globalDirsOne, List(ColumnDefinition(NamedColumnIdentifier("column1"))))) mustEqual Validated.Valid(true)
    }

    "fail if notRule is the same as value" in {
      val notRule = NotRule(Literal(Some("hello world")))
      notRule.evaluate(0, Row(List(Cell("hello world")), 1), Schema(globalDirsOne, List(ColumnDefinition(NamedColumnIdentifier("column1"))))) must beLike {
        case Validated.Invalid(messages) => messages.head mustEqual """not("hello world") fails for row: 1, column: column1, value: "hello world""""
      }
    }

    "fail with @ignoreCase" in {
      val notRule = NotRule(Literal(Some("hello world")))
      notRule.evaluate(0, Row(List(Cell("hello WORLD")), 1), Schema(globalDirsOne, List(ColumnDefinition(NamedColumnIdentifier("column1"), Nil, List(IgnoreCase()))))) must beLike {
        case Validated.Invalid(messages) => messages.head mustEqual """not("hello world") fails for row: 1, column: column1, value: "hello WORLD""""
      }
    }
  }
}