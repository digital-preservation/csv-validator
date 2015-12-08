/**
 * Copyright (c) 2013, The National Archives <digitalpreservation@nationalarchives.gov.uk>
 * http://www.nationalarchives.gov.uk
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package uk.gov.nationalarchives.csv.validator.schema.v1_1

import org.specs2.mutable.Specification
import uk.gov.nationalarchives.csv.validator.metadata.{Cell, Row}
import uk.gov.nationalarchives.csv.validator.schema._

import scalaz.{Failure, Success}

class AnyRuleSpec extends Specification {

  "AnyRule with a string literal behaviour" should  {
    val globalDirsOne = List(TotalColumns(1))
    val schema: Schema = Schema(globalDirsOne, List(ColumnDefinition(NamedColumnIdentifier("column1"))))

    "succeed if it matches the only any rule value" in {
      val anyRule = AnyRule(List(Literal(Some("hello world"))))
      anyRule.evaluate(0, Row(List(Cell("hello world")), 1), schema) mustEqual Success(true)
    }

    "succeed if it matches the one of any rule value" in {
      val anyRule = AnyRule(List(Literal(Some("hello world")), Literal(Some("value2")), Literal(Some("value3"))))
      anyRule.evaluate(0, Row(List(Cell("hello world")), 1), schema) mustEqual Success(true)
      anyRule.evaluate(0, Row(List(Cell("value2")), 2), schema) mustEqual Success(true)
      anyRule.evaluate(0, Row(List(Cell("value3")), 3), schema) mustEqual Success(true)
    }



    "fail if it doesn't matches" in {
      val anyRule = AnyRule(List(Literal(Some("hello world"))))

      anyRule.evaluate(0, Row(List(Cell("hello world today")), 1), schema) must beLike {
        case Failure(messages) => messages.head mustEqual """any("hello world") fails for line: 1, column: column1, value: "hello world today""""
      }
    }

    "succeed with @ignoreCase" in {
      val anyRule = AnyRule(List(Literal(Some("hello world"))))
      anyRule.evaluate(0, Row(List(Cell("hello WORLD")), 1), Schema(globalDirsOne, List(ColumnDefinition(NamedColumnIdentifier("column1"), Nil, List(IgnoreCase()))))) mustEqual Success(true)
    }
  }
}