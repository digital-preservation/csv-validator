/*
 * Copyright (c) 2013, The National Archives <digitalpreservation@nationalarchives.gov.uk>
 * https://www.nationalarchives.gov.uk
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

import cats.data.Validated

@RunWith(classOf[JUnitRunner])
class UniqueRuleSpec extends Specification {

  "unique rule" should {

    "pass if all column values are distinct" in {
      val schema = Schema(List(TotalColumns(1)), List(ColumnDefinition(NamedColumnIdentifier("Name"))))
      val rule = UniqueRule()

      rule.evaluate(0, Row(Cell("Jim") :: Nil, 1), schema)
      rule.evaluate(0, Row(Cell("Ben") :: Nil, 2), schema) must beLike { case Validated.Valid(_) => ok }
    }

    "fail if there are duplicate column values" in {
      val schema = Schema(List(TotalColumns(1)), List(ColumnDefinition(NamedColumnIdentifier("Name"))))
      val rule = UniqueRule()

      rule.evaluate(0, Row(Cell("Jim") :: Nil, 1), schema)
      rule.evaluate(0, Row(Cell("Ben") :: Nil, 2), schema)

      rule.evaluate(0, Row(Cell("Jim") :: Nil, 3), schema) must beLike {
        case Validated.Invalid(msgs) => msgs.toList mustEqual List("unique fails for line: 3, column: Name, value: \"Jim\" (original at line: 1)")
      }
    }

    "fail if columns differ only in case with @ignoreCase" in {
      val rule = UniqueRule()
      val schema = Schema(List(TotalColumns(1)), List(ColumnDefinition(NamedColumnIdentifier("Name"), rule :: Nil, IgnoreCase() :: Nil)))

      rule.evaluate(0, Row(Cell("Ben") :: Nil, 1), schema)

      rule.evaluate(0, Row(Cell("BEN") :: Nil, 2), schema) must beLike {
        case Validated.Invalid(msgs) => msgs.toList mustEqual List("unique fails for line: 2, column: Name, value: \"BEN\" (original at line: 1)")
      }
    }
  }
}
