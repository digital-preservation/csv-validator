/**
 * Copyright (c) 2013, The National Archives <digitalpreservation@nationalarchives.gov.uk>
 * http://www.nationalarchives.gov.uk
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package uk.gov.nationalarchives.csv.validator.schema.v1_0

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import uk.gov.nationalarchives.csv.validator.metadata.{Cell, Row}
import uk.gov.nationalarchives.csv.validator.schema._

import scalaz.{Failure, Success}

@RunWith(classOf[JUnitRunner])
class InRuleSpec extends Specification {

  "InRule with a string literal behaviour" should  {
    val globalDirsOne = List(TotalColumns(1))

    "succeed if inRule is embedded in value" in {
      val inRule = InRule(Literal(Some("myhello world today")))
      inRule.evaluate(0, Row(List(Cell("hello world")), 1), Schema(globalDirsOne, List(ColumnDefinition(NamedColumnIdentifier("column1"))))) mustEqual Success(true)
    }

    "succeed if inRule is the same as value" in {
      val inRule = InRule(Literal(Some("hello world")))
      inRule.evaluate(0, Row(List(Cell("hello world")), 1), Schema(globalDirsOne, List(ColumnDefinition(NamedColumnIdentifier("column1"))))) mustEqual Success(true)
    }

    "fail if inRule is not in value" in {
      val inRule = InRule(Literal(Some("hello world")))

      inRule.evaluate(0, Row(List(Cell("hello world today")), 1), Schema(globalDirsOne, List(ColumnDefinition(NamedColumnIdentifier("column1"))))) must beLike {
        case Failure(messages) => messages.head mustEqual """in("hello world") fails for line: 1, column: column1, value: "hello world today""""
      }
    }

    "succeed with @ignoreCase" in {
      val inRule = InRule(Literal(Some("hello world")))
      inRule.evaluate(0, Row(List(Cell("hello WORLD")), 1), Schema(globalDirsOne, List(ColumnDefinition(NamedColumnIdentifier("column1"), Nil, List(IgnoreCase()))))) mustEqual Success(true)
    }
  }
}