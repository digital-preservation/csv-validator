/**
 * Copyright (c) 2013, The National Archives <digitalpreservation@nationalarchives.gov.uk>
 * http://www.nationalarchives.gov.uk
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package uk.gov.nationalarchives.csv.validator.schema

import org.specs2.mutable.Specification
import uk.gov.nationalarchives.csv.validator.metadata.{Cell, Row}
import scalaz.{Failure, Success}

class EmptyRuleSpec extends Specification {

  val globalDirsOne = List(TotalColumns(1))

  "EmptyRule" should {

     "Succeed if cell is empty" in {
      val emptyRule = EmptyRule()
      emptyRule.evaluate(0, Row(List(Cell("")), 1), Schema(globalDirsOne, List(ColumnDefinition("column1")))) mustEqual Success(true)
     }

    "Fail if cell is NOT empty" in {
      val emptyRule = EmptyRule()
      emptyRule.evaluate(0, Row(List(Cell("something")), 1), Schema(globalDirsOne, List(ColumnDefinition("column1")))) must beLike {
        case Failure(messages) => messages.head mustEqual """empty fails for line: 1, column: column1, value: "something""""
      }
    }
  }
}
