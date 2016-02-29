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

import scalaz.{Failure, Success, IList}

@RunWith(classOf[JUnitRunner])
class UniqueMultiRuleSpec extends Specification {

  "unique multi rule" should {

    "succeed if all column values are distinct" in {
      val schema = Schema(List(TotalColumns(1)), List(ColumnDefinition(NamedColumnIdentifier("Name")), ColumnDefinition(NamedColumnIdentifier("Legs"))))
      val rule = UniqueMultiRule( ColumnReference(NamedColumnIdentifier("Legs")) :: Nil )

      rule.evaluate(0, Row(Cell("r2d2") :: Cell("3") :: Nil, 1), schema)
      rule.evaluate(0, Row(Cell("c3po") :: Cell("2") :: Nil, 2), schema) must beLike { case Success(_) => ok }
    }

    "succeed if duplicate column but 2nd is distinct" in {
      val schema = Schema(List(TotalColumns(1)), List(ColumnDefinition(NamedColumnIdentifier("Name")), ColumnDefinition(NamedColumnIdentifier("Legs"))))
      val rule = UniqueMultiRule( ColumnReference(NamedColumnIdentifier("Legs")) :: Nil )

      rule.evaluate(0, Row(Cell("r2d2") :: Cell("3") :: Nil, 1), schema)
      rule.evaluate(0, Row(Cell("r2d2") :: Cell("2") :: Nil, 2), schema) must beLike { case Success(_) => ok }
    }

    "fail if there are duplicate on all columns column values" in {
      val schema = Schema(List(TotalColumns(1)), List(ColumnDefinition(NamedColumnIdentifier("Name")), ColumnDefinition(NamedColumnIdentifier("Legs")), ColumnDefinition(NamedColumnIdentifier("Color"))))
      val rule = UniqueMultiRule( ColumnReference(NamedColumnIdentifier("Legs")) :: ColumnReference(NamedColumnIdentifier("Color")) :: Nil )

      rule.evaluate(0, Row(Cell("r2d2") :: Cell("3") :: Cell("blue") :: Nil, 1), schema)
      rule.evaluate(0, Row(Cell("r2d2") :: Cell("3") :: Cell("blue") :: Nil, 2), schema) must beLike {
        case Failure(msgs) => msgs.list mustEqual IList("unique( $Legs, $Color ) fails for line: 2, column: Name, value: \"r2d2\" (original at line: 1)")
      }
    }

  }
}
