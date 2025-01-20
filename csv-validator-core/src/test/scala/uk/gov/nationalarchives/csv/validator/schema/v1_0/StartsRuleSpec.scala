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
class StartsRuleSpec extends Specification {

  "StartsRule with a string literal behaviour" should  {
    val globalDirsOne = List(TotalColumns(1))

    "succeed if cell starts with startsRule value" in {
      val startsRule = StartsRule(Literal(Some("hello world")))

      startsRule.evaluate(0, Row(List(Cell("hello world today")), 1), Schema(globalDirsOne, List(ColumnDefinition(NamedColumnIdentifier("column1"))))) mustEqual Validated.Valid(true)
    }

    "fail if cell does not start with startsRule value" in {
      val startsRule = StartsRule(Literal(Some("hello world today")))
      startsRule.evaluate(0, Row(List(Cell("hello world")), 1), Schema(globalDirsOne, List(ColumnDefinition(NamedColumnIdentifier("column1"))))) must beLike {
        case Validated.Invalid(messages) => messages.head mustEqual """starts("hello world today") fails for line: 1, column: column1, value: "hello world""""
      }
    }

    "succeed if startsRule is the same as value" in {
      val startsRule = StartsRule(Literal(Some("hello world")))
      startsRule.evaluate(0, Row(List(Cell("hello world")), 1), Schema(globalDirsOne, List(ColumnDefinition(NamedColumnIdentifier("column1"))))) mustEqual Validated.Valid(true)
    }

    "succeed if startsRule's column reference does exist" in {
      val startsRule = StartsRule(ColumnReference(NamedColumnIdentifier("column1")))

      startsRule.evaluate(0, Row(List(Cell("hello world")), 1), Schema(globalDirsOne, List(ColumnDefinition(NamedColumnIdentifier("column1"))))) mustEqual Validated.Valid(true)
    }

    "fail if startsRule's column reference doesn't exist" in {
      val startsRule = StartsRule(ColumnReference(NamedColumnIdentifier("nonExistentColumn")))

      startsRule.evaluate(0, Row(List(Cell("hello world today")), 1), Schema(globalDirsOne, List(ColumnDefinition(NamedColumnIdentifier("column1"))))) must beLike {
        case Validated.Invalid(messages) => messages.head mustEqual """starts($nonExistentColumn) fails for line: 1, column: column1, value: "hello world today""""
      }
    }

    "succeed with @ignoreCase" in {
      val startsRule = StartsRule(Literal(Some("hello world")))
      startsRule.evaluate(0, Row(List(Cell("hello WORLD")), 1), Schema(globalDirsOne, List(ColumnDefinition(NamedColumnIdentifier("column1"), Nil, List(IgnoreCase()))))) mustEqual Validated.Valid(true)
    }
  }
}