/*
 * Copyright (c) 2013, The National Archives digitalpreservation@nationalarchives.gov.uk
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package uk.gov.tna.dri.schema

import org.specs2.mutable.Specification
import scalaz.{Success, Failure}
import uk.gov.tna.dri.metadata.{Cell, Row}

class LengthRuleSpec extends Specification {

  "Length rule" should  {

    "succeed with a matching single number" in {
      val lengthRule = new LengthRule(None, "5")

      lengthRule.evaluate(0, Row(List(Cell("Hello")), 1), Schema(List(TotalColumns(1), NoHeader()), List(ColumnDefinition("column1")))) mustEqual Success(true)
    }

    "succeed with a matching single '*'" in {
      val lengthRule = new LengthRule(None, "*")

      lengthRule.evaluate(0, Row(List(Cell("Hello")), 1), Schema(List(TotalColumns(1), NoHeader()), List(ColumnDefinition("column1")))) mustEqual Success(true)
    }

    "fail with a non-matching single number" in {
      val lengthRule = new LengthRule(None, "5")

      lengthRule.evaluate(0, Row(List(Cell("HelloWorld")), 1), Schema(List(TotalColumns(1), NoHeader()), List(ColumnDefinition("column1")))) must beLike {
        case Failure(m) => m.list mustEqual List("""length(5) fails for line: 1, column: column1, value: "HelloWorld"""")
      }
    }

    "succeed with valid range of numbers" in {
      val lengthRule = new LengthRule(Some("1"), "5")

      lengthRule.evaluate(0, Row(List(Cell("hello")), 1), Schema(List(TotalColumns(1), NoHeader()), List(ColumnDefinition("column1")))) mustEqual Success(true)
    }

    "succeed with valid range of numbers" in {
      val lengthRule = new LengthRule(Some("1"), "5")

      lengthRule.evaluate(0, Row(List(Cell("hello")), 1), Schema(List(TotalColumns(1), NoHeader()), List(ColumnDefinition("column1")))) mustEqual Success(true)
    }

    "fail with a out-of-range numbers" in {
      val lengthRule = new LengthRule(Some("1"), "5")

      lengthRule.evaluate(0, Row(List(Cell("helloworld")), 1), Schema(List(TotalColumns(1), NoHeader()), List(ColumnDefinition("column1")))) must beLike {
        case Failure(m) => m.list mustEqual List("""length(1,5) fails for line: 1, column: column1, value: "helloworld"""")
      }
    }

    "succeed with valid range and empty cell" in {
      val lengthRule = new LengthRule(Some("0"), "5")

      lengthRule.evaluate(0, Row(List(Cell("")), 1), Schema(List(TotalColumns(1), NoHeader()), List(ColumnDefinition("column1")))) mustEqual Success(true)
    }

    "succeed with cell within a non-0 length valid range " in {
      val lengthRule = new LengthRule(Some("5"), "10")

      lengthRule.evaluate(0, Row(List(Cell("HelloWorld")), 1), Schema(List(TotalColumns(1), NoHeader()), List(ColumnDefinition("column1")))) mustEqual Success(true)
    }


    "succeed with fixed size" in {
      val lengthRule = new LengthRule(Some("5"), "5")

      lengthRule.evaluate(0, Row(List(Cell("Hello")), 1), Schema(List(TotalColumns(1), NoHeader()), List(ColumnDefinition("column1")))) mustEqual Success(true)
    }

    "fail with a out-of-range for fixed size" in {
      val lengthRule = new LengthRule(Some("5"), "5")

      lengthRule.evaluate(0, Row(List(Cell("helloworld")), 1), Schema(List(TotalColumns(1), NoHeader()), List(ColumnDefinition("column1")))) must beLike {
        case Failure(m) => m.list mustEqual List("""length(5,5) fails for line: 1, column: column1, value: "helloworld"""")
      }
    }

    "succeed with '*' min length" in {
      val lengthRule = new LengthRule(Some("*"), "5")

      lengthRule.evaluate(0, Row(List(Cell("Hello")), 1), Schema(List(TotalColumns(1), NoHeader()), List(ColumnDefinition("column1")))) mustEqual Success(true)
    }

    "succeed with '*' max length" in {
      val lengthRule = new LengthRule(Some("5"), "*")

      lengthRule.evaluate(0, Row(List(Cell("HelloWorld")), 1), Schema(List(TotalColumns(1), NoHeader()), List(ColumnDefinition("column1")))) mustEqual Success(true)
    }

    "succeed with '*' min & max length" in {
      val lengthRule = new LengthRule(Some("*"), "*")

      lengthRule.evaluate(0, Row(List(Cell("HelloWorld")), 1), Schema(List(TotalColumns(1), NoHeader()), List(ColumnDefinition("column1")))) mustEqual Success(true)
    }


    "fail with length is longer than cell" in {
      val lengthRule = new LengthRule(None, "43")

      lengthRule.evaluate(0, Row(List(Cell("helloworld")), 1), Schema(List(TotalColumns(1), NoHeader()), List(ColumnDefinition("column1")))) must beLike {
        case Failure(m) => m.list mustEqual List("""length(43) fails for line: 1, column: column1, value: "helloworld"""")
      }
    }

  }
}