/*
 * Copyright (c) 2013, The National Archives digitalpreservation@nationalarchives.gov.uk
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package uk.gov.tna.dri.csv.validator.schema

import org.specs2.mutable.Specification
import scalaz.{Failure, Success}
import uk.gov.tna.dri.csv.validator.metadata.{Cell, Row}

/**
 * User: Jim Collins
 * Date: 2/20/13
 */
class NoArgRulesSpec extends Specification {

  val globalDirsOne = List(TotalColumns(1))

  "UriRule" should  {

    "succeed if cell has a valid HomeGuard style uri" in {
      val uriRule = UriRule()
      uriRule.evaluate(0, Row(List(Cell("http://datagov.nationalarchives.gov.uk/66/WO/409/9999/0/aaaaaaaa-aaaa-4aaa-9eee-0123456789ab")), 1), Schema(globalDirsOne, List(ColumnDefinition("column1")))) mustEqual Success(true)
    }

    "succeed if cell has a valid uri without scheme" in {
      val uriRule = UriRule()
      uriRule.evaluate(0, Row(List(Cell("datagov.nationalarchives.gov.uk/66/WO/409/9999/0/aaaaaaaa-aaaa-4aaa-9eee-0123456789ab")), 1), Schema(globalDirsOne, List(ColumnDefinition("column1")))) mustEqual Success(true)
    }

    "succeed if cell has a valid uri without scheme and host" in {
      val uriRule = UriRule()
      uriRule.evaluate(0, Row(List(Cell("/66/WO/409/9999/0/aaaaaaaa-aaaa-4aaa-9eee-0123456789ab")), 1), Schema(globalDirsOne, List(ColumnDefinition("column1")))) mustEqual Success(true)
    }

    "succeed if cell has a valid relative uri without scheme and host" in {
      val uriRule = UriRule()
      uriRule.evaluate(0, Row(List(Cell("66/WO/409/9999/0/aaaaaaaa-aaaa-4aaa-9eee-0123456789ab")), 1), Schema(globalDirsOne, List(ColumnDefinition("column1")))) mustEqual Success(true)
    }

    "succeed if cell has a valid File system style uri" in {
      val uriRule = UriRule()
      uriRule.evaluate(0, Row(List(Cell("file:///WO/16/409/27_1/1/thing.jp2")), 1), Schema(globalDirsOne, List(ColumnDefinition("column1")))) mustEqual Success(true)
    }

    "fail if cell has an invalid uri" in {

      val uriRule = UriRule()
      uriRule.evaluate(0, Row(List(Cell("http://http://datagov.nationalarchives.gov.uk/66/WO/409/9999/0/aaaaaaaa-aaaa-4aaa-9eee-0123456789ab")), 1), Schema(globalDirsOne, List(ColumnDefinition("column1")))) must beLike {
        case Failure(messages) => messages.head mustEqual """uri fails for line: 1, column: column1, value: "http://http://datagov.nationalarchives.gov.uk/66/WO/409/9999/0/aaaaaaaa-aaaa-4aaa-9eee-0123456789ab""""
      }
    }
  }

  "Uuid4Rule" should  {

    "succeed if cell has a valid uuid4" in {
      val uuid4Rule = Uuid4Rule()
      uuid4Rule.evaluate(0, Row(List(Cell("aaaaaaaa-aaaa-4aaa-9eee-0123456789ab")), 1), Schema(globalDirsOne, List(ColumnDefinition("column1")))) mustEqual Success(true)
    }

    "fail if cell has an invalid uuid4" in {
      val uuid4Rule = Uuid4Rule()
      uuid4Rule.evaluate(0, Row(List(Cell("aaaaaaaaa-aaaa-4aaa-9eee-0123456789ab")), 1), Schema(globalDirsOne, List(ColumnDefinition("column1")))) must beLike {
        case Failure(messages) => messages.head mustEqual """uuid4 fails for line: 1, column: column1, value: "aaaaaaaaa-aaaa-4aaa-9eee-0123456789ab""""
      }
    }
  }

  "PositiveIntegerRule" should  {

    "succeed if cell has a positive integer" in {
      val posIntRule = PositiveIntegerRule()
      posIntRule.evaluate(0, Row(List(Cell("120912459")), 1), Schema(globalDirsOne, List(ColumnDefinition("column1")))) must beLike { case Success(_) => ok }
    }

    "succeed if cell has a single digit positive integer" in {
      val posIntRule = PositiveIntegerRule()
      posIntRule.evaluate(0, Row(List(Cell("3")), 1), Schema(globalDirsOne, List(ColumnDefinition("column1")))) must beLike { case Success(_) => ok }
    }

    "fail if cell has a negative integer" in {
      val posIntRule = PositiveIntegerRule()
      posIntRule.evaluate(0, Row(List(Cell("-123")), 1), Schema(globalDirsOne, List(ColumnDefinition("column1")))) must beLike {
        case Failure(messages) => messages.head mustEqual """positiveInteger fails for line: 1, column: column1, value: "-123""""
      }
    }

    "fail if cell has a non integer" in {
      val posIntRule = PositiveIntegerRule()
      posIntRule.evaluate(0, Row(List(Cell("123.45")), 1), Schema(globalDirsOne, List(ColumnDefinition("column1")))) must beLike {
        case Failure(messages) => messages.head mustEqual """positiveInteger fails for line: 1, column: column1, value: "123.45""""
      }
    }

    "succeed for cell with a leading zero" in {
      val posIntRule = PositiveIntegerRule()
      posIntRule.evaluate(0, Row(List(Cell("0123")), 1), Schema(globalDirsOne, List(ColumnDefinition("column1")))) must beLike { case Success(_) => ok }
    }

    "fail if cell has a minus sign midway through" in {
      val posIntRule = PositiveIntegerRule()
      posIntRule.evaluate(0, Row(List(Cell("123-4456")), 1), Schema(globalDirsOne, List(ColumnDefinition("column1")))) must beLike {
        case Failure(messages) => messages.head mustEqual """positiveInteger fails for line: 1, column: column1, value: "123-4456""""
      }
    }

    "fail if cell has a non numeric character" in {
      val posIntRule = PositiveIntegerRule()
      posIntRule.evaluate(0, Row(List(Cell("12abc45")), 1), Schema(globalDirsOne, List(ColumnDefinition("column1")))) must beLike {
        case Failure(messages) => messages.head mustEqual """positiveInteger fails for line: 1, column: column1, value: "12abc45""""
      }
    }
  }
}
