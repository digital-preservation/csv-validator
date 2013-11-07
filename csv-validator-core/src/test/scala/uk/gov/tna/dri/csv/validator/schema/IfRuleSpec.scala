/*
 * Copyright (c) 2013, The National Archives digitalpreservation@nationalarchives.gov.uk
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package uk.gov.tna.dri.csv.validator.schema

import org.specs2.mutable.Specification
import uk.gov.tna.dri.csv.validator.metadata.{Cell, Row}
import scalaz.{Failure, Success}

/**
 * User: Jim Collins
 * Date: 3/7/13
 */
class IfRuleSpec extends Specification {

  "IfRule" should  {
    val globalDirsOne = List(TotalColumns(1))

    "condition and 'if body' is true and no 'else'" in {
      val ifRule = IfRule(StartsRule(Literal(Some("hello world"))),List(EndsRule(Literal(Some("world")))), None)
      ifRule.evaluate(0, Row(List(Cell("hello world")), 1), Schema(globalDirsOne, List(ColumnDefinition("column1")))) mustEqual Success(List(true))
    }

    "condition is true and no else, 'if body' rule fails" in {
      val ifRule = IfRule(StartsRule(Literal(Some("hello world"))),List(EndsRule(Literal(Some("hello")))), None)
      ifRule.evaluate(0, Row(List(Cell("hello world")), 1), Schema(globalDirsOne, List(ColumnDefinition("column1")))) must beLike {
        case Failure(messages) => messages.head mustEqual "ends(\"hello\") fails for line: 1, column: column1, value: \"hello world\""
      }
    }

    "condition is true and there is an 'else' that is true and 'if' body that is false" in {
      val ifRule = IfRule(StartsRule(Literal(Some("hello"))),List(EndsRule(Literal(Some("hello")))), Some(List(EndsRule(Literal(Some("world"))))))
      ifRule.evaluate(0, Row(List(Cell("hello world")), 1), Schema(globalDirsOne, List(ColumnDefinition("column1")))) must beLike {
        case Failure(messages) => messages.head mustEqual "ends(\"hello\") fails for line: 1, column: column1, value: \"hello world\""
      }
    }

    "condition is false and there is no 'else' and 'if' body that is true" in {
      val ifRule = IfRule(StartsRule(Literal(Some("sello"))),List(EndsRule(Literal(Some("world")))), None)
      ifRule.evaluate(0, Row(List(Cell("hello world")), 1), Schema(globalDirsOne, List(ColumnDefinition("column1")))) mustEqual Success(List())
    }

    "condition is false and there is an 'else' that is true and body that is true" in {
      val ifRule = IfRule(StartsRule(Literal(Some("sello"))),List(EndsRule(Literal(Some("world")))), Some(List(EndsRule(Literal(Some("world"))))))
      ifRule.evaluate(0, Row(List(Cell("hello world")), 1), Schema(globalDirsOne, List(ColumnDefinition("column1")))) mustEqual Success(List(true))
    }

    "condition is false and there is an 'else' that is false and 'if' body that is true" in {
      val ifRule = IfRule(StartsRule(Literal(Some("sello"))),List(EndsRule(Literal(Some("world")))), Some(List(EndsRule(Literal(Some("hello"))))))
      ifRule.evaluate(0, Row(List(Cell("hello world")), 1), Schema(globalDirsOne, List(ColumnDefinition("column1")))) must beLike {
        case Failure(messages) => messages.head mustEqual "ends(\"hello\") fails for line: 1, column: column1, value: \"hello world\""
      }
    }

    "condition is false and 'else' has nested 'if' that is false with no 'else'" in {
      val ifRule = IfRule(StartsRule(Literal(Some("sello"))),List(EndsRule(Literal(Some("world")))),
        Some(List(
          IfRule(StartsRule(Literal(Some("sello"))),List(EndsRule(Literal(Some("world")))), None)
        ))
      )
      ifRule.evaluate(0, Row(List(Cell("hello world")), 1), Schema(globalDirsOne, List(ColumnDefinition("column1")))) mustEqual Success(List(List()))
    }

    "condition is false and 'else' has nested 'if' that is true with no 'else'" in {
      val ifRule = IfRule(StartsRule(Literal(Some("sello"))),List(EndsRule(Literal(Some("world")))),
        Some(List(
          IfRule(StartsRule(Literal(Some("hello"))),List(EndsRule(Literal(Some("world")))), None)
        ))
      )
      ifRule.evaluate(0, Row(List(Cell("hello world")), 1), Schema(globalDirsOne, List(ColumnDefinition("column1")))) mustEqual Success(List(List(true)))
    }

    "nested if'" in {
      val ifRule = IfRule(
        StartsRule(Literal(Some("u"))),List(EndsRule(Literal(Some("def")))),
        Some(List(
          IfRule(
            StartsRule(Literal(Some("u")))
            ,List(EndsRule(Literal(Some("world")))), Some(List(EndsRule(Literal(Some("world"))))))
        ))
      )
      ifRule.evaluate(0, Row(List(Cell("hello world")), 1), Schema(globalDirsOne, List(ColumnDefinition("column1")))) mustEqual Success(List(List(true)))
    }

  }
}
