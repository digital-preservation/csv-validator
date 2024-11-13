/*
 * Copyright (c) 2013, The National Archives <digitalpreservation@nationalarchives.gov.uk>
 * https://www.nationalarchives.gov.uk
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package uk.gov.nationalarchives.csv.validator.schema.v1_0

import org.specs2.mutable.Specification
import uk.gov.nationalarchives.csv.validator.metadata.{Cell, Row}
import uk.gov.nationalarchives.csv.validator.schema._

import cats.data.Validated

class OrRuleSpec extends Specification {

  "OrRule" should {
    "succeed when left rule validates" in {
      val globalDirectives = List(TotalColumns(1))
      val schema = Schema(globalDirectives, List(ColumnDefinition(NamedColumnIdentifier("Country"))))

      val leftInRule = InRule(Literal(Some("Germany")))
      val rightInRule = InRule(Literal(Some("France")))

      val orRule = OrRule(leftInRule, rightInRule)

      orRule.evaluate(0, Row(List(Cell("Germany")), 1), schema) mustEqual Validated.Valid(true)
    }

    "succeed when right rule validates" in {
      val globalDirectives = List(TotalColumns(1))
      val schema = Schema(globalDirectives, List(ColumnDefinition(NamedColumnIdentifier("Country"))))

      val leftInRule = InRule(Literal(Some("Germany")))
      val rightInRule = InRule(Literal(Some("France")))

      val orRule = OrRule(leftInRule, rightInRule)

      orRule.evaluate(0, Row(List(Cell("France")), 1), schema) mustEqual Validated.Valid(true)
    }

    "fail when left/right rules are invalid" in {
      val globalDirectives = List(TotalColumns(1))
      val schema = Schema(globalDirectives, List(ColumnDefinition(NamedColumnIdentifier("ThisOrThat"))))

      val leftInRule = InRule(Literal(Some("This")))
      val rightInRule = InRule(Literal(Some("That")))

      val orRule = OrRule(leftInRule, rightInRule)

      orRule.evaluate(0, Row(List(Cell("SomethingElse")), 1), schema) must beLike {
        case Validated.Invalid(messages) => messages.toList mustEqual List("""in("This") or in("That") fails for line: 1, column: ThisOrThat, value: "SomethingElse"""")
      }
    }

    "fail when left cross reference rule is invalid and right rule is invalid" in {
      val globalDirectives = List(TotalColumns(1))
      val schema = Schema(globalDirectives, List(ColumnDefinition(NamedColumnIdentifier("Country"))))

      val leftInRule = InRule(ColumnReference(NamedColumnIdentifier("ConfigurableCountry")))
      val rightInRule = InRule(Literal(Some("France")))

      val orRule = OrRule(leftInRule, rightInRule)

      orRule.evaluate(0, Row(List(Cell("UK")), 1), schema) must throwA[NoSuchElementException]
    }

    "succeed when 3 'or' rules valid for right rule" in {
      val globalDirectives = List(TotalColumns(1))
      val schema = Schema(globalDirectives, List(ColumnDefinition(NamedColumnIdentifier("Direction"))))

      val leftInRule = InRule(Literal(Some("left")))
      val middleInRule = InRule(Literal(Some("middle")))
      val rightInRule = InRule(Literal(Some("right")))

      val orRule =  OrRule( OrRule(leftInRule, middleInRule), rightInRule )

      orRule.evaluate(0, Row(List(Cell("right")), 1), schema) mustEqual Validated.Valid(true)
    }

    "succeed when 3 'or' rules valid for left/middle rule" in {
      val globalDirectives = List(TotalColumns(1))
      val schema = Schema(globalDirectives, List(ColumnDefinition(NamedColumnIdentifier("Direction"))))

      val leftInRule = InRule(Literal(Some("left")))
      val middleInRule = InRule(Literal(Some("middle")))
      val rightInRule = InRule(Literal(Some("right")))

      val orRule =  OrRule( OrRule(leftInRule, middleInRule), rightInRule )

      orRule.evaluate(0, Row(List(Cell("middle")), 1), schema) mustEqual Validated.Valid(true)
    }

    "fail when all 3 'or' rules are invalid " in {
      val globalDirectives = List(TotalColumns(1))
      val schema = Schema(globalDirectives, List(ColumnDefinition(NamedColumnIdentifier("Direction"))))

      val leftInRule = InRule(Literal(Some("left")))
      val middleInRule = InRule(Literal(Some("middle")))
      val rightInRule = InRule(Literal(Some("right")))

      val orRule =  OrRule( OrRule(leftInRule, middleInRule), rightInRule )

      orRule.evaluate(0, Row(List(Cell("up")), 1), schema) must beLike {
        case Validated.Invalid(messages) => messages.toList mustEqual List("""in("left") or in("middle") or in("right") fails for line: 1, column: Direction, value: "up"""")
      }
    }
  }
}