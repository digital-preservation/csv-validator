/**
 * Copyright (c) 2013, The National Archives <digitalpreservation@nationalarchives.gov.uk>
 * https://www.nationalarchives.gov.uk
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package uk.gov.nationalarchives.csv.validator.schema

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import uk.gov.nationalarchives.csv.validator.metadata.{Cell, Row}
import uk.gov.nationalarchives.csv.validator.schema.v1_0._
import uk.gov.nationalarchives.csv.validator.schema.v1_1.{XsdDateTimeWithTimeZoneRangeRule, XsdDateTimeWithTimeZoneRule}

//import uk.gov.nationalarchives.csv.validator.schema.v1_1._
import scalaz.{Failure, Success}

/**
 * User: Jim Collins
 * Date: 3/15/13
 */
@RunWith(classOf[JUnitRunner])
class DateRulesSpec extends Specification {

  val globalDirsOne = List(TotalColumns(1))

  val schema: Schema = Schema(globalDirsOne, List(ColumnDefinition(NamedColumnIdentifier("column1"))))

  "XsdDateTimeRule" should  {

    "succeed if cell has a valid xsdDateTime" in {
      val xsdDateRule = XsdDateTimeRule()
      xsdDateRule.evaluate(0, Row(List(Cell("2002-12-30T09:00:10")), 1), schema) mustEqual Success(true)
    }

    "succeed if cell has a valid xsdDateTime with explicit zero'd timezone" in {
      val xsdDateRule = XsdDateTimeRule()
      xsdDateRule.evaluate(0, Row(List(Cell("2013-03-22T11:33:40+00:00")), 1), schema) mustEqual Success(true)
    }

    "fail if cell has an invalid xsdDateTime format" in {
      val xsdDateRule = XsdDateTimeRule()
      xsdDateRule.evaluate(0, Row(List(Cell("2002-999-30T09:00:10")), 1), schema) must beLike {
        case Failure(messages) => messages.head mustEqual """xDateTime fails for line: 1, column: column1, value: "2002-999-30T09:00:10""""
      }
    }

    "fail if cell has a valid xsdDateTime format but invalid date" in {
      val xsdDateRule = XsdDateTimeRule()
      xsdDateRule.evaluate(0, Row(List(Cell("2002-99-30T09:00:10")), 1), schema) must beLike {
        case Failure(messages) => messages.head mustEqual """xDateTime fails for line: 1, column: column1, value: "2002-99-30T09:00:10""""
      }
    }

  }

  "XsdDateTimeRangeRule" should  {

    "succeed if cell is equal to the lower bound" in {
      val xsdDateRangeRule = XsdDateTimeRangeRule("2012-01-01T01:00:00", "2013-01-01T01:00:00")
      xsdDateRangeRule.evaluate(0, Row(List(Cell("2012-01-01T01:00:00")), 1), schema) mustEqual Success(true)
    }

    "succeed if cell is between lower bound and upper bound" in {
      val xsdDateRangeRule = XsdDateTimeRangeRule("2012-01-01T00:00:00", "2013-01-01T00:00:00")
      xsdDateRangeRule.evaluate(0, Row(List(Cell("2012-12-12T00:00:00")), 1), schema) mustEqual Success(true)
    }

    "pass if cell is equal to upper bound" in {
      val xsdDateRangeRule = XsdDateTimeRangeRule("2012-01-01T00:00:00", "2013-01-01T00:00:00")
      xsdDateRangeRule.evaluate(0, Row(List(Cell("2013-01-01T00:00:00")), 1), schema) mustEqual Success(true)
    }

    "succeed if cell just below upper bound" in {
      val xsdDateRangeRule = XsdDateTimeRangeRule("2012-01-01T00:00:00", "2013-01-01T00:00:01")
      xsdDateRangeRule.evaluate(0, Row(List(Cell("2013-01-01T00:00:00")), 1), schema) mustEqual Success(true)
    }

    "fail if cell is just below lower bound" in {
      val xsdDateRangeRule = XsdDateTimeRangeRule("2012-01-01T00:00:01", "2013-01-01T00:00:00")
      xsdDateRangeRule.evaluate(0, Row(List(Cell("2012-01-01T00:00:00")), 1), schema) must beLike {
        case Failure(messages) => messages.head mustEqual """xDateTime("2012-01-01T00:00:01, 2013-01-01T00:00:00") fails for line: 1, column: column1, value: "2012-01-01T00:00:00""""
      }
    }
  }

  "XsdDateTimeWithTimeZoneRule" should  {

    "succeed if cell has a valid xsdDateTimeWithTimeZone" in {
      val xsdDateRule = XsdDateTimeWithTimeZoneRule()
      xsdDateRule.evaluate(0, Row(List(Cell("2002-12-30T09:00:10Z")), 1), schema) mustEqual Success(true)
      xsdDateRule.evaluate(0, Row(List(Cell("2002-12-30T09:00:10+01:00")), 2), schema) mustEqual Success(true)
      xsdDateRule.evaluate(0, Row(List(Cell("2002-12-30T09:00:10-01:00")), 3), schema) mustEqual Success(true)
    }


    "fail if cell has an invalid xsdDateTimeWithTimeZone format" in {
      val xsdDateRule = XsdDateTimeWithTimeZoneRule()
      xsdDateRule.evaluate(0, Row(List(Cell("2002-999-30T09:00:10Z")), 1), schema) must beLike {
        case Failure(messages) => messages.head mustEqual """xDateTimeWithTimeZone fails for line: 1, column: column1, value: "2002-999-30T09:00:10Z""""
      }
    }

    "fail if cell has an  xsdDateTimeWithTimeZone missing time zone" in {
      val xsdDateRule = XsdDateTimeWithTimeZoneRule()
      xsdDateRule.evaluate(0, Row(List(Cell("2002-01-30T09:00:10")), 1), schema) must beLike {
        case Failure(messages) => messages.head mustEqual """xDateTimeWithTimeZone fails for line: 1, column: column1, value: "2002-01-30T09:00:10""""
      }
    }

  }

  "XsdDateTimeWithTimeZoneRangeRule" should  {

    "succeed if cell is equal to the lower bound" in {
      val xsdDateRangeRule = XsdDateTimeWithTimeZoneRangeRule("2012-01-01T01:00:00Z", "2013-01-01T01:00:00Z")
      xsdDateRangeRule.evaluate(0, Row(List(Cell("2012-01-01T01:00:00Z")), 1), schema) mustEqual Success(true)
      xsdDateRangeRule.evaluate(0, Row(List(Cell("2012-01-01T01:00:00+00:00")), 2), schema) mustEqual Success(true)
      xsdDateRangeRule.evaluate(0, Row(List(Cell("2012-01-01T01:00:00-00:00")), 3), schema) mustEqual Success(true)
    }

    "succeed if cell is between lower bound and upper bound" in {
      val xsdDateRangeRule = XsdDateTimeWithTimeZoneRangeRule("2012-01-01T00:00:00+00:00", "2013-01-01T00:00:00Z")
      xsdDateRangeRule.evaluate(0, Row(List(Cell("2012-12-12T00:00:00Z")), 1), schema) mustEqual Success(true)
      xsdDateRangeRule.evaluate(0, Row(List(Cell("2012-12-12T00:00:00+00:00")), 2), schema) mustEqual Success(true)
    }

    "pass if cell is equal to upper bound" in {
      val xsdDateRangeRule = XsdDateTimeWithTimeZoneRangeRule("2012-01-01T00:00:00Z", "2013-01-01T00:00:00+00:00")
      xsdDateRangeRule.evaluate(0, Row(List(Cell("2013-01-01T00:00:00Z")), 1), schema) mustEqual Success(true)
      xsdDateRangeRule.evaluate(0, Row(List(Cell("2013-01-01T01:00:00+01:00")), 2), schema) mustEqual Success(true)
      xsdDateRangeRule.evaluate(0, Row(List(Cell("2012-12-31T23:00:00-01:00")), 3), schema) mustEqual Success(true)
    }

    "succeed if cell just below upper bound" in {
      val xsdDateRangeRule = XsdDateTimeWithTimeZoneRangeRule("2012-01-01T00:00:00Z", "2013-01-01T00:00:01Z")
      xsdDateRangeRule.evaluate(0, Row(List(Cell("2013-01-01T00:00:00Z")), 1), schema) mustEqual Success(true)
      xsdDateRangeRule.evaluate(0, Row(List(Cell("2013-01-01T01:00:00+01:00")), 2), schema) mustEqual Success(true)
      xsdDateRangeRule.evaluate(0, Row(List(Cell("2012-12-31T23:00:00-01:00")), 3), schema) mustEqual Success(true)
    }

    "fail if cell is just below lower bound" in {
      val xsdDateRangeRule = XsdDateTimeWithTimeZoneRangeRule("2012-01-01T00:00:01Z", "2013-01-01T00:00:00Z")
      xsdDateRangeRule.evaluate(0, Row(List(Cell("2012-01-01T00:00:00Z")), 1), schema) must beLike {
        case Failure(messages) => messages.head mustEqual """xDateTimeWithTimeZone("2012-01-01T00:00:01Z, 2013-01-01T00:00:00Z") fails for line: 1, column: column1, value: "2012-01-01T00:00:00Z""""
      }
    }
  }

  "XsdDateRule" should  {

    "succeed if cell has a valid xsdDate" in {
      val xsdDateRule = XsdDateRule()
      xsdDateRule.evaluate(0, Row(List(Cell("2002-01-30")), 1), schema) mustEqual Success(true)
    }

    "succeed if cell has a valid xsdDate with explicit zero'd timezone" in {
      val xsdDateRule = XsdDateRule()
      xsdDateRule.evaluate(0, Row(List(Cell("2013-03-22+00:00")), 1), schema) mustEqual Success(true)
    }

    "fail if cell has an invalid xsdDate format" in {
      val xsdDateRule = XsdDateRule()
      xsdDateRule.evaluate(0, Row(List(Cell("2002-999-30")), 1), schema) must beLike {
        case Failure(messages) => messages.head mustEqual """xDate fails for line: 1, column: column1, value: "2002-999-30""""
      }
    }

    "fail if cell has a valid xsdDate format but invalid date" in {
      val xsdDateRule = XsdDateRule()
      xsdDateRule.evaluate(0, Row(List(Cell("2002-99-30")), 1), schema) must beLike {
        case Failure(messages) => messages.head mustEqual """xDate fails for line: 1, column: column1, value: "2002-99-30""""
      }
    }
  }

  "XsdDateRangeRule" should  {

    "succeed if cell is equal to the lower bound" in {
      val xsdDateRangeRule = XsdDateRangeRule("2012-01-01", "2013-01-01")
      xsdDateRangeRule.evaluate(0, Row(List(Cell("2012-01-01")), 1), schema) mustEqual Success(true)
    }

    "succeed if cell is between lower bound and upper bound" in {
      val xsdDateRangeRule = XsdDateRangeRule("2012-01-01", "2013-01-01")
      xsdDateRangeRule.evaluate(0, Row(List(Cell("2012-12-12")), 1), schema) mustEqual Success(true)
    }

    "pass if cell is equal to upper bound" in {
      val xsdDateRangeRule = XsdDateRangeRule("2012-01-01", "2013-01-01")
      xsdDateRangeRule.evaluate(0, Row(List(Cell("2013-01-01")), 1), schema) mustEqual Success(true)
    }

    "succeed if cell just below upper bound" in {
      val xsdDateRangeRule = XsdDateRangeRule("2012-01-01", "2013-01-01")
      xsdDateRangeRule.evaluate(0, Row(List(Cell("2013-01-01")), 1), schema) mustEqual Success(true)
    }

    "fail if cell is just below lower bound" in {
      val xsdDateRangeRule = XsdDateRangeRule("2012-01-01", "2013-01-01")
      xsdDateRangeRule.evaluate(0, Row(List(Cell("2011-12-31")), 1), schema) must beLike {
        case Failure(messages) => messages.head mustEqual """xDate("2012-01-01, 2013-01-01") fails for line: 1, column: column1, value: "2011-12-31""""
      }
    }
  }

  "partUkDateRule" should {
    "succeed if cell has a valid UK Date" in {
      val partUkDateRule = PartUkDateRule()
      partUkDateRule.evaluate(0, Row(List(Cell("04/February/1981")), 1), schema) mustEqual Success(true)
    }

    "succeed if cell has a valid UK Date with missing day" in {
      val partUkDateRule = PartUkDateRule()
      partUkDateRule.evaluate(0, Row(List(Cell("*/February/1981")), 1), schema) mustEqual Success(true)
    }

    "succeed if cell has a valid UK Date with missing month" in {
      val partUkDateRule = PartUkDateRule()
      partUkDateRule.evaluate(0, Row(List(Cell("04/*/1981")), 1), schema) mustEqual Success(true)
    }

    "succeed if cell has a valid UK Date with missing year" in {
      val partUkDateRule = PartUkDateRule()
      partUkDateRule.evaluate(0, Row(List(Cell("04/February/*")), 1), schema) mustEqual Success(true)
    }

    "succeed if cell has a valid UK Date with missing day, month and year" in {
      val partUkDateRule = PartUkDateRule()
      partUkDateRule.evaluate(0, Row(List(Cell("*/*/*")), 1), schema) mustEqual Success(true)
    }

    "succeed if cell has a valid UK Date with illegiable day symbol" in {
      val partUkDateRule = PartUkDateRule()
      partUkDateRule.evaluate(0, Row(List(Cell("1?/June/2013")), 1), schema) mustEqual Success(true)
    }

    "succeed if cell has a valid UK Date with double illegiable day" in {
      val partUkDateRule = PartUkDateRule()
      partUkDateRule.evaluate(0, Row(List(Cell("??/June/2013")), 1), schema) mustEqual Success(true)
    }

    "succeed if cell has a valid UK Date with illegiable month" in {
      val partUkDateRule = PartUkDateRule()
      partUkDateRule.evaluate(0, Row(List(Cell("15/?/2013")), 1), schema) mustEqual Success(true)
    }

    "succeed if cell has a valid UK Date with illegiable day, month and year" in {
      val partUkDateRule = PartUkDateRule()
      partUkDateRule.evaluate(0, Row(List(Cell("?2/?/201?")), 1), schema) mustEqual Success(true)
    }

    "fail if cell has a valid UK Date with wrong ? count for month" in {
      val partUkDateRule = PartUkDateRule()
      partUkDateRule.evaluate(0, Row(List(Cell("?2/???/201?")), 1), schema) must beLike {
        case Failure(messages) => messages.head mustEqual """partUkDate fails for line: 1, column: column1, value: "?2/???/201?""""
      }
    }

    "fail if cell has a valid UK Date with wrong number of year symbols" in {
      val partUkDateRule = PartUkDateRule()
      partUkDateRule.evaluate(0, Row(List(Cell("04/Feburary/81")), 1), schema) must beLike {
        case Failure(messages) => messages.head mustEqual """partUkDate fails for line: 1, column: column1, value: "04/Feburary/81""""
      }
    }

    "fail if cell has a valid UK Date with typo in month" in {
      val partUkDateRule = PartUkDateRule()
      partUkDateRule.evaluate(0, Row(List(Cell("09/Augudt/2010")), 1), schema) must beLike {
        case Failure(messages) => messages.head mustEqual """partUkDate fails for line: 1, column: column1, value: "09/Augudt/2010""""
      }
    }

    "fail if cell has a valid UK Date with - instead of /" in {
      val partUkDateRule = PartUkDateRule()
      partUkDateRule.evaluate(0, Row(List(Cell("09-August-2010")), 1), schema) must beLike {
        case Failure(messages) => messages.head mustEqual """partUkDate fails for line: 1, column: column1, value: "09-August-2010""""
      }
    }

    "fail if cell has a valid UK Date with where zero padding is missing" in {
      val partUkDateRule = PartUkDateRule()
      partUkDateRule.evaluate(0, Row(List(Cell("9/August/2010")), 1), schema) must beLike {
        case Failure(messages) => messages.head mustEqual """partUkDate fails for line: 1, column: column1, value: "9/August/2010""""
      }
    }
  }

  "UkDateRule" should  {

    "succeed if cell has a valid UK Date" in {
      val ukDateRule = UkDateRule()
      ukDateRule.evaluate(0, Row(List(Cell("30/12/2012")), 1), schema) mustEqual Success(true)
    }

    "fail if cell has an invalid UK Date format" in {
      val ukDateRule = UkDateRule()
      ukDateRule.evaluate(0, Row(List(Cell("990/00/0009")), 1), schema) must beLike {
        case Failure(messages) => messages.head mustEqual """ukDate fails for line: 1, column: column1, value: "990/00/0009""""
      }
    }

    "fail if cell has a valid UK Date format but invalid date" in {
      val ukDateRule = UkDateRule()
      ukDateRule.evaluate(0, Row(List(Cell("99/12/2012")), 1), schema) must beLike {
        case Failure(messages) => messages.head mustEqual """ukDate fails for line: 1, column: column1, value: "99/12/2012""""
      }
    }

  }

  "UkDateRangeRule" should  {

    "succeed if cell is equal to the lower bound" in {
      val ukDateRangeRule = UkDateRangeRule("01/01/2012", "01/01/2013")
      ukDateRangeRule.evaluate(0, Row(List(Cell("01/01/2012")), 1), schema) mustEqual Success(true)
    }

    "succeed if cell is between lower bound and upper bound" in {
      val ukDateRangeRule = UkDateRangeRule("01/01/2012", "01/01/2013")
      ukDateRangeRule.evaluate(0, Row(List(Cell("12/05/2012")), 1), schema) mustEqual Success(true)
    }

    "pass if cell is equal to upper bound" in {
      val ukDateRangeRule = UkDateRangeRule("01/01/2012", "01/01/2013")
      ukDateRangeRule.evaluate(0, Row(List(Cell("01/01/2013")), 1), schema) mustEqual Success(true)
    }

    "succeed if cell just below upper bound" in {
      val ukDateRangeRule = UkDateRangeRule("01/01/2012", "02/01/2013")
      ukDateRangeRule.evaluate(0, Row(List(Cell("01/01/2013")), 1), schema) mustEqual Success(true)
    }

    "fail if cell is just below lower bound" in {
      val ukDateRangeRule = UkDateRangeRule("01/01/2012", "02/01/2013")
      ukDateRangeRule.evaluate(0, Row(List(Cell("12/31/2011")), 1), schema) must beLike {
        case Failure(messages) => messages.head mustEqual """ukDate("01/01/2012, 02/01/2013") fails for line: 1, column: column1, value: "12/31/2011""""
      }
    }
  }

  "XsdTimeRule" should  {

    "succeed if cell has a valid xsdTime" in {
      val xsdTimeRule = XsdTimeRule()
      xsdTimeRule.evaluate(0, Row(List(Cell("12:30:20")), 1), schema) mustEqual Success(true)
    }

    "succeed if cell has a valid xsdTime with explicit zero'd timezone" in {
      val xsdTimeRule = XsdTimeRule()
      xsdTimeRule.evaluate(0, Row(List(Cell("11:33:40+00:00")), 1), schema) mustEqual Success(true)
    }

    "fail if cell has an invalid xsdTime" in {
      val xsdTimeRule = XsdTimeRule()
      xsdTimeRule.evaluate(0, Row(List(Cell("99:000:88")), 1), schema) must beLike {
        case Failure(messages) => messages.head mustEqual """xTime fails for line: 1, column: column1, value: "99:000:88""""
      }
    }
  }

  "XsdTimeRangeRule" should  {

    "succeed if cell is equal to the lower bound" in {
      val xsdTimeRangeRule = XsdTimeRangeRule("01:10:01", "01:10:10")
      xsdTimeRangeRule.evaluate(0, Row(List(Cell("01:10:01")), 1), schema) mustEqual Success(true)
    }

    "succeed if cell is between lower bound and upper bound" in {
      val xsdTimeRangeRule = XsdTimeRangeRule("01:10:01", "01:20:10")
      xsdTimeRangeRule.evaluate(0, Row(List(Cell("01:15:01")), 1), schema) mustEqual Success(true)
    }

    "pass if cell is equal to upper bound" in {
      val xsdTimeRangeRule = XsdTimeRangeRule("01:10:01", "01:10:10")
      xsdTimeRangeRule.evaluate(0, Row(List(Cell("01:10:10")), 1), schema) mustEqual Success(true)
    }

    "succeed if cell just below upper bound" in {
      val xsdTimeRangeRule = XsdTimeRangeRule("01:10:01", "01:10:10")
      xsdTimeRangeRule.evaluate(0, Row(List(Cell("01:10:09")), 1), schema) mustEqual Success(true)
    }

    "fail if cell is just below lower bound" in {
      val xsdTimeRangeRule = XsdTimeRangeRule("01:10:01", "01:10:10")
      xsdTimeRangeRule.evaluate(0, Row(List(Cell("01:10:00")), 1), schema) must beLike {
        case Failure(messages) => messages.head mustEqual """xTime("01:10:01, 01:10:10") fails for line: 1, column: column1, value: "01:10:00""""
      }
    }
  }

}
