/**
 * Copyright (c) 2013, The National Archives <digitalpreservation@nationalarchives.gov.uk>
 * http://www.nationalarchives.gov.uk
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package uk.gov.nationalarchives.csv.validator.schema.v1_1

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import uk.gov.nationalarchives.csv.validator.metadata.{Cell, Row}
import uk.gov.nationalarchives.csv.validator.schema.{ColumnDefinition, NamedColumnIdentifier, Schema, TotalColumns}

import scalaz.{Failure, Success}

@RunWith(classOf[JUnitRunner])
class IdenticalRuleSpec extends Specification {

  "IdenticalRule" should  {
    val globalDirsTwo = List(TotalColumns(2))

    "succeed when there is only one non empty row" in {
      val identicalRule = IdenticalRule()
      val schema = Schema(globalDirsTwo, List(ColumnDefinition(NamedColumnIdentifier("column1")), ColumnDefinition(NamedColumnIdentifier("column2"))))
      identicalRule.evaluate(0, Row(List(Cell("row1"),Cell("abc")), 1), schema) mustEqual Success(true)
    }

    "succeed when all rows in the column are equal" in {
      val identicalRule = IdenticalRule()
      val schema = Schema(globalDirsTwo, List(ColumnDefinition(NamedColumnIdentifier("column1")), ColumnDefinition(NamedColumnIdentifier("column2"))))
      identicalRule.evaluate(0, Row(List(Cell("row1"),Cell("abc")), 1), schema) mustEqual Success(true)
      identicalRule.evaluate(0, Row(List(Cell("row1"),Cell("efd")), 2), schema) mustEqual Success(true)
      identicalRule.evaluate(0, Row(List(Cell("row1"),Cell("ghi")), 3), schema) mustEqual Success(true)
    }

    "fail when rows in the column are different" in {
      val identicalRule1 = IdenticalRule()
      val schema = Schema(globalDirsTwo, List(ColumnDefinition(NamedColumnIdentifier("column1")), ColumnDefinition(NamedColumnIdentifier("column2"))))
      identicalRule1.evaluate(1, Row(List(Cell("row1"),Cell("abc")), 1), schema) mustEqual Success(true)
      identicalRule1.evaluate(1, Row(List(Cell("row1"),Cell("efd")), 2), schema) must beLike {
        case Failure(msgs) => msgs.list mustEqual(List("identical fails for line: 2, column: column2, value: \"efd\""))
      }
      identicalRule1.evaluate(1, Row(List(Cell("row1"),Cell("ghi")), 3), schema) must beLike {
        case Failure(msgs) => msgs.list mustEqual(List("identical fails for line: 3, column: column2, value: \"ghi\""))
      }

      val identicalRule2 = IdenticalRule()
      identicalRule2.evaluate(0, Row(List(Cell("row1"),Cell("abc")), 1), schema) mustEqual Success(true)
      identicalRule2.evaluate(0, Row(List(Cell("row1"),Cell("efd")), 2), schema) mustEqual Success(true)
      identicalRule2.evaluate(0, Row(List(Cell("row2"),Cell("ghi")), 3), schema) must beLike {
        case Failure(msgs) => msgs.list mustEqual(List("identical fails for line: 3, column: column1, value: \"row2\""))
      }
    }
    
    "fail when rows are empty" in {
      val identicalRule = IdenticalRule()
      val schema = Schema(globalDirsTwo, List(ColumnDefinition(NamedColumnIdentifier("column1")), ColumnDefinition(NamedColumnIdentifier("column2"))))
      identicalRule.evaluate(0, Row(List(Cell(""),Cell("abc")), 1), schema) must beLike {
        case Failure(msgs) => msgs.list mustEqual(List("identical fails for line: 1, column: column1, value: \"\""))
      }
      identicalRule.evaluate(0, Row(List(Cell(""),Cell("efd")), 2), schema) must beLike {
        case Failure(msgs) => msgs.list mustEqual(List("identical fails for line: 2, column: column1, value: \"\""))
      }
      identicalRule.evaluate(0, Row(List(Cell(""),Cell("ghi")), 3), schema) must beLike {
        case Failure(msgs) => msgs.list mustEqual(List("identical fails for line: 3, column: column1, value: \"\""))
      }
    }


  }
}
