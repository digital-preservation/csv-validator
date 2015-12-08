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
import uk.gov.nationalarchives.csv.validator.schema.v1_0.{EndsRule, StartsRule}

import scalaz.{Failure, Success}

class SwitchRuleSpec extends Specification {

  "SwitchRule" should  {
    val globalDirsOne = List(TotalColumns(1))
    val schema: Schema = Schema(globalDirsOne, List(ColumnDefinition(NamedColumnIdentifier("column1"))))

    val row: Row = Row(List(Cell("hello world")), 1)

    def startsRule(prefix: String): StartsRule = StartsRule(Literal(Some(prefix)))
    def endsRules(suffix: String): List[EndsRule] = List(EndsRule(Literal(Some(suffix))))

    "condition1 and body1 are true and no 'else'" in {
      val switchRule = SwitchRule(None,(startsRule("hello world"),endsRules("world")))
      switchRule.evaluate(0, row, schema) mustEqual Success(List(true))
    }


    "condition1  true and no else,  body1 rule fails" in {
      val switchRule = SwitchRule(None,(startsRule("hello world"),endsRules("hello")))
      switchRule.evaluate(0, row, schema) must beLike {
        case Failure(messages) => messages.head mustEqual "ends(\"hello\") fails for line: 1, column: column1, value: \"hello world\""
      }
    }

    "condition1 is true and there is an 'else' that is true and body1 that is false" in {
      val switchRule = SwitchRule(Some(endsRules("world")),(startsRule("hello"),endsRules("hello")))
      switchRule.evaluate(0, row, schema) must beLike {
        case Failure(messages) => messages.head mustEqual "ends(\"hello\") fails for line: 1, column: column1, value: \"hello world\""
      }
    }


    "condition1 is false and there is no 'else' and body1 that is true" in {
      val switchRule = SwitchRule(None,(startsRule("sello"),endsRules("world")))
      switchRule.evaluate(0, row, schema) mustEqual Success(List())
    }

    "condition1 is false and there is an 'else' that is true and body1 that is true" in {
      val switchRule = SwitchRule(Some(endsRules("world")),(startsRule("sello"),endsRules("world")))
      switchRule.evaluate(0, row, schema) mustEqual Success(List(true))
    }

    "condition1 is false and there is an 'else' that is false and body1 that is true" in {
      val switchRule = SwitchRule(Some(endsRules("hello")),(startsRule("sello"),endsRules("world")))
      switchRule.evaluate(0, row, schema) must beLike {
        case Failure(messages) => messages.head mustEqual "ends(\"hello\") fails for line: 1, column: column1, value: \"hello world\""
      }
    }

    "condition1 and condition2 are false with no 'else'" in {
      val switchRule = SwitchRule(None, (startsRule("sello"),endsRules("world")), (startsRule("sello"),endsRules("world")) )
      switchRule.evaluate(0, row, schema) mustEqual Success(List())
    }

    "condition1 is false and condition2 is true with no 'else'" in {
      val switchRule = SwitchRule(None, (startsRule("sello"),endsRules("world")), (startsRule("hello"),endsRules("world")) )
      switchRule.evaluate(0, row, schema) mustEqual Success((List(true)))
    }

    "condition1 and condition2 are false with true else condition" in {
      val switchRule = SwitchRule(Some(endsRules("world")), (startsRule("sello"),endsRules("world")), (startsRule("sello"),endsRules("world")) )
      switchRule.evaluate(0, row, schema) mustEqual Success(List(true))
    }

    "condition1, condition2 and condition3 are false with no 'else'" in {
      val switchRule = SwitchRule(None, (startsRule("sello"),endsRules("world")), (startsRule("sello"),endsRules("world")), (startsRule("sello"),endsRules("world")) )
      switchRule.evaluate(0, row, schema) mustEqual Success(List())
    }

    "condition1, condition2 and condition3  are false with true else condition" in {
      val switchRule = SwitchRule(Some(endsRules("world")), (startsRule("sello"),endsRules("world")), (startsRule("sello"),endsRules("world")), (startsRule("sello"),endsRules("world")) )
      switchRule.evaluate(0, row, schema) mustEqual Success(List(true))
    }


  }
}
