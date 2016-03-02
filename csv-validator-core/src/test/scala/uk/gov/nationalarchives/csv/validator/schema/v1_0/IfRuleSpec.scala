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

/**
  * User: Jim Collins
  * Date: 3/7/13
  */
@RunWith(classOf[JUnitRunner])
class IfRuleSpec extends Specification {

  "IfRule" should  {
    val globalDirsOne = List(TotalColumns(1))
    val schema: Schema = Schema(globalDirsOne, List(ColumnDefinition(NamedColumnIdentifier("column1"))))

    val row: Row = Row(List(Cell("hello world")), 1)

    def startsRule(prefix: String): StartsRule = StartsRule(Literal(Some(prefix)))
    def endsRules(suffix: String): List[EndsRule] = List(EndsRule(Literal(Some(suffix))))

    "condition and 'if body' is true and no 'else'" in {
      val ifRule = IfRule(startsRule("hello world"),endsRules("world"), None)
      ifRule.evaluate(0, row, schema) mustEqual Success(List(true))
    }

    "condition is true and no else, 'if body' rule fails" in {
      val ifRule = IfRule(startsRule("hello world"),endsRules("hello"), None)
      ifRule.evaluate(0, row, schema) must beLike {
        case Failure(messages) => messages.head mustEqual "ends(\"hello\") fails for line: 1, column: column1, value: \"hello world\""
      }
    }

    "condition is true and there is an 'else' that is true and 'if' body that is false" in {
      val ifRule = IfRule(startsRule("hello"),endsRules("hello"), Some(endsRules("world")))
      ifRule.evaluate(0, row, schema) must beLike {
        case Failure(messages) => messages.head mustEqual "ends(\"hello\") fails for line: 1, column: column1, value: \"hello world\""
      }
    }

    "condition is false and there is no 'else' and 'if' body that is true" in {
      val ifRule = IfRule(startsRule("sello"),endsRules("world"), None)
      ifRule.evaluate(0, row, schema) mustEqual Success(List())
    }

    "condition is false and there is an 'else' that is true and body that is true" in {
      val ifRule = IfRule(StartsRule(Literal(Some("sello"))),endsRules("world"), Some(endsRules("world")))
      ifRule.evaluate(0, row, schema) mustEqual Success(List(true))
    }

    "condition is false and there is an 'else' that is false and 'if' body that is true" in {
      val ifRule = IfRule(startsRule("sello"),endsRules("world"), Some(endsRules("hello")))
      ifRule.evaluate(0, row, schema) must beLike {
        case Failure(messages) => messages.head mustEqual "ends(\"hello\") fails for line: 1, column: column1, value: \"hello world\""
      }
    }

    "condition is false and 'else' has nested 'if' that is false with no 'else'" in {
      val ifRule = IfRule(startsRule("sello"),endsRules("world"),
        Some(List(
          IfRule(startsRule("sello"),endsRules("world"), None)
        ))
      )
      ifRule.evaluate(0, row, schema) mustEqual Success(List(List()))
    }

    "condition is false and 'else' has nested 'if' that is true with no 'else'" in {
      val ifRule = IfRule(startsRule("sello"),endsRules("world"),
        Some(List(
          IfRule(startsRule("hello"),endsRules("world"), None)
        ))
      )
      ifRule.evaluate(0, row, schema) mustEqual Success(List(List(true)))
    }

    "nested if'" in {
      val ifRule = IfRule(
        startsRule("u"),endsRules("def"),
        Some(List(
          IfRule(
            startsRule("u")
            ,endsRules("world"), Some(endsRules("world")))
        ))
      )
      ifRule.evaluate(0, row, schema) mustEqual Success(List(List(true)))
    }

  }
}
