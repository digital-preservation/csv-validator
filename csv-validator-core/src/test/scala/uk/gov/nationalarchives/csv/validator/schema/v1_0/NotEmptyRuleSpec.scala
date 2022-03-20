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
import uk.gov.nationalarchives.csv.validator.schema.{ColumnDefinition, NamedColumnIdentifier, Schema, TotalColumns}

import scalaz.{Failure, Success}

@RunWith(classOf[JUnitRunner])
class NotEmptyRuleSpec extends Specification {

   val globalDirsOne = List(TotalColumns(1))

   "NotEmptyRule" should {

     "File if cell is empty" in {
       val notEmptyRule = NotEmptyRule()
       notEmptyRule.evaluate(0, Row(List(Cell("")), 1), Schema(globalDirsOne, List(ColumnDefinition(NamedColumnIdentifier("column1"))))) must beLike {
         case Failure(messages) => messages.head mustEqual """notEmpty fails for line: 1, column: column1, value: """""
       }


      }

     "Succeed if cell is NOT empty" in {
       val notEmptyRule = NotEmptyRule()
       notEmptyRule.evaluate(0, Row(List(Cell("something")), 1), Schema(globalDirsOne, List(ColumnDefinition(NamedColumnIdentifier("column1"))))) mustEqual Success(true)
     }
   }
 }
