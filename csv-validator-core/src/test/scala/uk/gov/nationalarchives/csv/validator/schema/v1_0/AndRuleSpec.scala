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
class AndRuleSpec extends Specification {

  "AndRule" should {
    "fail when left rule only validates" in {
      val globalDirectives = List(TotalColumns(1))
      val schema = Schema(globalDirectives, List(ColumnDefinition(NamedColumnIdentifier("Country"))))

      val leftInRule = InRule(Literal(Some("Germany")))
      val rightInRule = InRule(Literal(Some("France")))

      val andRule = AndRule(leftInRule, rightInRule)

      andRule.evaluate(0, Row(List(Cell("Germany")), 1), schema) must beLike {
        case Validated.Invalid(messages) => messages.toList mustEqual List("""in("Germany") and in("France") fails for row: 1, column: Country, value: "Germany"""")
      }
    }

    "fail when left rule only validates" in {
      val globalDirectives = List(TotalColumns(1))
      val schema = Schema(globalDirectives, List(ColumnDefinition(NamedColumnIdentifier("Country"))))

      val leftInRule = InRule(Literal(Some("Germany")))
      val rightInRule = InRule(Literal(Some("France")))

      val andRule = AndRule(leftInRule, rightInRule)

      andRule.evaluate(0, Row(List(Cell("France")), 1), schema) must beLike {
        case Validated.Invalid(messages) => messages.toList mustEqual List("""in("Germany") and in("France") fails for row: 1, column: Country, value: "France"""")
      }
    }

    "fail when left/right rules are both invalid" in {
      val globalDirectives = List(TotalColumns(1))
      val schema = Schema(globalDirectives, List(ColumnDefinition(NamedColumnIdentifier("ThisOrThat"))))

      val leftInRule = InRule(Literal(Some("This")))
      val rightInRule = InRule(Literal(Some("That")))

      val andRule = AndRule(leftInRule, rightInRule)

      andRule.evaluate(0, Row(List(Cell("SomethingElse")), 1), schema) must beLike {
        case Validated.Invalid(messages) => messages.toList mustEqual List("""in("This") and in("That") fails for row: 1, column: ThisOrThat, value: "SomethingElse"""")
      }
    }

    "succeed when both left and right match" in {
      val globalDirectives = List(TotalColumns(1))
      val schema = Schema(globalDirectives, List(ColumnDefinition(NamedColumnIdentifier("Country"))))

      val leftInRule = InRule(Literal(Some("UK")))
      val rightInRule = InRule(Literal(Some("UK")))

      val andRule = AndRule(leftInRule, rightInRule)

      andRule.evaluate(0, Row(List(Cell("UK")), 1), schema) mustEqual Validated.Valid(true)
    }

    "succeed when both left and parentheses right match" in {
      val globalDirectives = List(TotalColumns(1))
      val schema = Schema(globalDirectives, List(ColumnDefinition(NamedColumnIdentifier("Country"))))

      val pLeftInRule = IsRule(Literal(Some("UK")))
      val pRightInRule = IsRule(Literal(Some("UK")))

      val leftInRule = IsRule(Literal(Some("UK")))
      val rightInRule = ParenthesesRule(pLeftInRule :: pRightInRule :: Nil )

      val andRule = AndRule(leftInRule, rightInRule)

      andRule.evaluate(0, Row(List(Cell("UK")), 1), schema) mustEqual Validated.Valid(List(true,true))
    }

    "succeed when both left and parentheses right match" in {
      val globalDirectives = List(TotalColumns(1))
      val schema = Schema(globalDirectives, List(ColumnDefinition(NamedColumnIdentifier("Country"))))

      val pLeftInRule = IsRule(Literal(Some("UK")))
      val pRightInRule = IsRule(Literal(Some("UK1")))

      val leftInRule = IsRule(Literal(Some("UK")))
      val rightInRule = ParenthesesRule(pLeftInRule :: pRightInRule :: Nil )

      val andRule = AndRule(leftInRule, rightInRule)

      andRule.evaluate(0, Row(List(Cell("SomethingElse")), 1), schema) must beLike {
        case Validated.Invalid(messages) => messages.toList mustEqual List("""is("UK") and (is("UK") is("UK1")) fails for row: 1, column: Country, value: "SomethingElse"""")
      }
    }
  }
}