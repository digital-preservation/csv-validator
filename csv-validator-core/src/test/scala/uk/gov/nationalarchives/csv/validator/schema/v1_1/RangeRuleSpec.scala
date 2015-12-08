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
import uk.gov.nationalarchives.csv.validator.schema.{ColumnDefinition, NamedColumnIdentifier, Schema, TotalColumns}

import scalaz.{Failure, Success}


class RangeRuleSpec extends Specification {

  "RangeRule" should  {
    val globalDirectives = List(TotalColumns(1))
    val schema = Schema(globalDirectives, List(ColumnDefinition(NamedColumnIdentifier("Country"))))

    "fail when non numeric number passed" in {
      val rangeRule = new RangeRule(Some(1),Some(2))

      rangeRule.evaluate(0, Row(List(Cell("Germany")), 1), schema) must beLike {
        case Failure(messages) => messages.list mustEqual List("""range(1,2) fails for line: 1, column: Country, value: "Germany"""")
      }
    }

    "pass when we test integer boundaries" in {
      val rangeRule = new RangeRule(Some(Int.MinValue),Some(Int.MaxValue))

      rangeRule.evaluate(0, Row(List(Cell((Int.MaxValue).toString)), 1), schema)  mustEqual Success(true)
    }

    "pass when we test integer boundaries with no max limit" in {
      val rangeRule = new RangeRule(Some(0) ,None)

      rangeRule.evaluate(0, Row(List(Cell(BigDecimal(Int.MaxValue).toString)), 1), schema)  mustEqual Success(true)
    }

    "pass when we test integer boundaries with no min limit" in {
      val rangeRule = new RangeRule(None,Some(0) )

      rangeRule.evaluate(0, Row(List(Cell(BigDecimal(Int.MinValue).toString)), 1), schema)  mustEqual Success(true)
    }

    "fail when we test small decimal outside range" in {
      val rangeRule = new RangeRule(Some(0.01),Some(0.1))

      rangeRule.evaluate(0, Row(List(Cell(("0.00999999999999999999999999999999"))), 1), schema)  must beLike {
        case Failure(messages) => messages.list mustEqual List("""range(0.01,0.1) fails for line: 1, column: Country, value: "0.00999999999999999999999999999999"""")
      }
    }
  }
}
