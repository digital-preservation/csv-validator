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

class UniqueMultiRuleSpec extends Specification {

  "unique multi rule" should {

    "succeed if all column values are distinct" in {
      val schema = Schema(List(TotalColumns(1)), List(ColumnDefinition(NamedColumnIdentifier("Name")), ColumnDefinition(NamedColumnIdentifier("Legs"))))
      val rule = UniqueMultiRule( ColumnReference(NamedColumnIdentifier("Legs")) :: Nil )

      rule.evaluate(0, Row(Cell("r2d2") :: Cell("3") :: Nil, 1), schema)
      rule.evaluate(0, Row(Cell("c3po") :: Cell("2") :: Nil, 2), schema) must beLike { case Validated.Valid(_) => ok }
    }

    "succeed if duplicate column but 2nd is distinct" in {
      val schema = Schema(List(TotalColumns(1)), List(ColumnDefinition(NamedColumnIdentifier("Name")), ColumnDefinition(NamedColumnIdentifier("Legs"))))
      val rule = UniqueMultiRule( ColumnReference(NamedColumnIdentifier("Legs")) :: Nil )

      rule.evaluate(0, Row(Cell("r2d2") :: Cell("3") :: Nil, 1), schema)
      rule.evaluate(0, Row(Cell("r2d2") :: Cell("2") :: Nil, 2), schema) must beLike { case Validated.Valid(_) => ok }
    }

    "fail if there are duplicate on all columns column values" in {
      val schema = Schema(List(TotalColumns(1)), List(ColumnDefinition(NamedColumnIdentifier("Name")), ColumnDefinition(NamedColumnIdentifier("Legs")), ColumnDefinition(NamedColumnIdentifier("Color"))))
      val rule = UniqueMultiRule( ColumnReference(NamedColumnIdentifier("Legs")) :: ColumnReference(NamedColumnIdentifier("Color")) :: Nil )

      rule.evaluate(0, Row(Cell("r2d2") :: Cell("3") :: Cell("blue") :: Nil, 1), schema)
      rule.evaluate(0, Row(Cell("r2d2") :: Cell("3") :: Cell("blue") :: Nil, 2), schema) must beLike {
        case Validated.Invalid(msgs) => msgs.toList mustEqual List("unique( $Legs, $Color ) fails for line: 2, column: Name, value: \"r2d2\" (original at line: 1)")
      }
    }

  }
}
