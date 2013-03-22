package uk.gov.tna.dri.schema

import org.specs2.mutable.Specification
import uk.gov.tna.dri.metadata.{Cell, Row}
import scalaz.{Failure, Success}

/**
 * User: Jim Collins
 * Date: 3/15/13
 */
class DateRulesSpec extends Specification {

  val globalDirsOne = List(TotalColumns(1))

  "XsdDateTimeRule" should  {

    "succeed if cell has a valid xsdDateTime" in {
      val xsdDateRule = XsdDateTimeRule()
      xsdDateRule.evaluate(0, Row(List(Cell("2002-12-30T09:00:10")), 1), Schema(globalDirsOne, List(ColumnDefinition("column1")))) mustEqual Success(true)
    }

    "fail if cell has an invalid xsdDateTime format" in {
      val xsdDateRule = XsdDateTimeRule()
      xsdDateRule.evaluate(0, Row(List(Cell("2002-999-30T09:00:10")), 1), Schema(globalDirsOne, List(ColumnDefinition("column1")))) must beLike {
        case Failure(messages) => messages.head mustEqual """xDateTime fails for line: 1, column: column1, value: "2002-999-30T09:00:10""""
      }
    }

    "fail if cell has a valid xsdDateTime format but invalid date" in {
      val xsdDateRule = XsdDateTimeRule()
      xsdDateRule.evaluate(0, Row(List(Cell("2002-99-30T09:00:10")), 1), Schema(globalDirsOne, List(ColumnDefinition("column1")))) must beLike {
        case Failure(messages) => messages.head mustEqual """xDateTime fails for line: 1, column: column1, value: "2002-99-30T09:00:10""""
      }
    }

  }

  "XsdDateTimeRangeRule" should  {

    "succeed if cell is equal to the lower bound" in {
      val xsdDateRangeRule = XsdDateTimeRangeRule("2012-01-01T01:00:00", "2013-01-01T01:00:00")
      xsdDateRangeRule.evaluate(0, Row(List(Cell("2012-01-01T01:00:00")), 1), Schema(globalDirsOne, List(ColumnDefinition("column1")))) mustEqual Success(true)
    }

    "succeed if cell is between lower bound and upper bound" in {
      val xsdDateRangeRule = XsdDateTimeRangeRule("2012-01-01T00:00:00", "2013-01-01T00:00:00")
      xsdDateRangeRule.evaluate(0, Row(List(Cell("2012-12-12T00:00:00")), 1), Schema(globalDirsOne, List(ColumnDefinition("column1")))) mustEqual Success(true)
    }

    "pass if cell is equal to upper bound" in {
      val xsdDateRangeRule = XsdDateTimeRangeRule("2012-01-01T00:00:00", "2013-01-01T00:00:00")
      xsdDateRangeRule.evaluate(0, Row(List(Cell("2013-01-01T00:00:00")), 1), Schema(globalDirsOne, List(ColumnDefinition("column1")))) mustEqual Success(true)
    }

    "succeed if cell just below upper bound" in {
      val xsdDateRangeRule = XsdDateTimeRangeRule("2012-01-01T00:00:00", "2013-01-01T00:00:01")
      xsdDateRangeRule.evaluate(0, Row(List(Cell("2013-01-01T00:00:00")), 1), Schema(globalDirsOne, List(ColumnDefinition("column1")))) mustEqual Success(true)
    }

    "fail if cell is just below lower bound" in {
      val xsdDateRangeRule = XsdDateTimeRangeRule("2012-01-01T00:00:01", "2013-01-01T00:00:00")
      xsdDateRangeRule.evaluate(0, Row(List(Cell("2012-01-01T00:00:00")), 1), Schema(globalDirsOne, List(ColumnDefinition("column1")))) must beLike {
        case Failure(messages) => messages.head mustEqual """xDateTime("2012-01-01T00:00:01, 2013-01-01T00:00:00") fails for line: 1, column: column1, value: "2012-01-01T00:00:00""""
      }
    }
  }

  "XsdDateRule" should  {

    "succeed if cell has a valid xsdDate" in {
      val xsdDateRule = XsdDateRule()
      xsdDateRule.evaluate(0, Row(List(Cell("2002-01-30")), 1), Schema(globalDirsOne, List(ColumnDefinition("column1")))) mustEqual Success(true)
    }

    "fail if cell has an invalid xsdDate format" in {
      val xsdDateRule = XsdDateRule()
      xsdDateRule.evaluate(0, Row(List(Cell("2002-999-30")), 1), Schema(globalDirsOne, List(ColumnDefinition("column1")))) must beLike {
        case Failure(messages) => messages.head mustEqual """xDate fails for line: 1, column: column1, value: "2002-999-30""""
      }
    }

    "fail if cell has a valid xsdDate format but invalid date" in {
      val xsdDateRule = XsdDateRule()
      xsdDateRule.evaluate(0, Row(List(Cell("2002-99-30")), 1), Schema(globalDirsOne, List(ColumnDefinition("column1")))) must beLike {
        case Failure(messages) => messages.head mustEqual """xDate fails for line: 1, column: column1, value: "2002-99-30""""
      }
    }
  }

  "XsdDateRangeRule" should  {

    "succeed if cell is equal to the lower bound" in {
      val xsdDateRangeRule = XsdDateRangeRule("2012-01-01T00:00:00", "2013-01-01T00:00:00")
      xsdDateRangeRule.evaluate(0, Row(List(Cell("2012-01-01")), 1), Schema(globalDirsOne, List(ColumnDefinition("column1")))) mustEqual Success(true)
    }

    "succeed if cell is between lower bound and upper bound" in {
      val xsdDateRangeRule = XsdDateRangeRule("2012-01-01T00:00:00", "2013-01-01T00:00:00")
      xsdDateRangeRule.evaluate(0, Row(List(Cell("2012-12-12")), 1), Schema(globalDirsOne, List(ColumnDefinition("column1")))) mustEqual Success(true)
    }

    "pass if cell is equal to upper bound" in {
      val xsdDateRangeRule = XsdDateRangeRule("2012-01-01", "2013-01-01")
      xsdDateRangeRule.evaluate(0, Row(List(Cell("2013-01-01")), 1), Schema(globalDirsOne, List(ColumnDefinition("column1")))) mustEqual Success(true)
    }

    "succeed if cell just below upper bound" in {
      val xsdDateRangeRule = XsdDateRangeRule("2012-01-01T00:00:00", "2013-01-01T00:00:01")
      xsdDateRangeRule.evaluate(0, Row(List(Cell("2013-01-01")), 1), Schema(globalDirsOne, List(ColumnDefinition("column1")))) mustEqual Success(true)
    }

    "fail if cell is just below lower bound" in {
      val xsdDateRangeRule = XsdDateRangeRule("2012-01-01T00:00:01", "2013-01-01T00:00:00")
      xsdDateRangeRule.evaluate(0, Row(List(Cell("2012-01-01")), 1), Schema(globalDirsOne, List(ColumnDefinition("column1")))) must beLike {
        case Failure(messages) => messages.head mustEqual """xDate("2012-01-01T00:00:01, 2013-01-01T00:00:00") fails for line: 1, column: column1, value: "2012-01-01""""
      }
    }
  }

  "UkDateRule" should  {

    "succeed if cell has a valid UK Date" in {
      val ukDateRule = UkDateRule()
      ukDateRule.evaluate(0, Row(List(Cell("30/12/2012")), 1), Schema(globalDirsOne, List(ColumnDefinition("column1")))) mustEqual Success(true)
    }

    "fail if cell has an invalid UK Date format" in {
      val ukDateRule = UkDateRule()
      ukDateRule.evaluate(0, Row(List(Cell("990/00/0009")), 1), Schema(globalDirsOne, List(ColumnDefinition("column1")))) must beLike {
        case Failure(messages) => messages.head mustEqual """ukDate fails for line: 1, column: column1, value: "990/00/0009""""
      }
    }

    "fail if cell has a valid UK Date format but invalid date" in {
      val ukDateRule = UkDateRule()
      ukDateRule.evaluate(0, Row(List(Cell("99/12/2012")), 1), Schema(globalDirsOne, List(ColumnDefinition("column1")))) must beLike {
        case Failure(messages) => messages.head mustEqual """ukDate fails for line: 1, column: column1, value: "99/12/2012""""
      }
    }

  }

  "UkDateRangeRule" should  {

    "succeed if cell is equal to the lower bound" in {
      val ukDateRangeRule = UkDateRangeRule("01/01/2012", "01/01/2013")
      ukDateRangeRule.evaluate(0, Row(List(Cell("01/01/2012")), 1), Schema(globalDirsOne, List(ColumnDefinition("column1")))) mustEqual Success(true)
    }

    "succeed if cell is between lower bound and upper bound" in {
      val ukDateRangeRule = UkDateRangeRule("01/01/2012", "01/01/2013")
      ukDateRangeRule.evaluate(0, Row(List(Cell("12/05/2012")), 1), Schema(globalDirsOne, List(ColumnDefinition("column1")))) mustEqual Success(true)
    }

    "pass if cell is equal to upper bound" in {
      val ukDateRangeRule = UkDateRangeRule("01/01/2012", "01/01/2013")
      ukDateRangeRule.evaluate(0, Row(List(Cell("01/01/2013")), 1), Schema(globalDirsOne, List(ColumnDefinition("column1")))) mustEqual Success(true)
    }

    "succeed if cell just below upper bound" in {
      val ukDateRangeRule = UkDateRangeRule("01/01/2012", "02/01/2013")
      ukDateRangeRule.evaluate(0, Row(List(Cell("01/01/2013")), 1), Schema(globalDirsOne, List(ColumnDefinition("column1")))) mustEqual Success(true)
    }

    "fail if cell is just below lower bound" in {
      val ukDateRangeRule = UkDateRangeRule("01/01/2012", "02/01/2013")
      ukDateRangeRule.evaluate(0, Row(List(Cell("12/31/2011")), 1), Schema(globalDirsOne, List(ColumnDefinition("column1")))) must beLike {
        case Failure(messages) => messages.head mustEqual """ukDate("01/01/2012, 02/01/2013") fails for line: 1, column: column1, value: "12/31/2011""""
      }
    }
  }

  "XsdTimeRule" should  {

    "succeed if cell has a valid xsdTime" in {
      val xsdTimeRule = XsdTimeRule()
      xsdTimeRule.evaluate(0, Row(List(Cell("12:30:20")), 1), Schema(globalDirsOne, List(ColumnDefinition("column1")))) mustEqual Success(true)
    }

    "fail if cell has an invalid xsdTime" in {
      val xsdTimeRule = XsdTimeRule()
      xsdTimeRule.evaluate(0, Row(List(Cell("99:000:88")), 1), Schema(globalDirsOne, List(ColumnDefinition("column1")))) must beLike {
        case Failure(messages) => messages.head mustEqual """xTime fails for line: 1, column: column1, value: "99:000:88""""
      }
    }
  }

  "XsdTimeRangeRule" should  {

    "succeed if cell is equal to the lower bound" in {
      val xsdTimeRangeRule = XsdTimeRangeRule("01:10:01", "01:10:10")
      xsdTimeRangeRule.evaluate(0, Row(List(Cell("01:10:01")), 1), Schema(globalDirsOne, List(ColumnDefinition("column1")))) mustEqual Success(true)
    }

    "succeed if cell is between lower bound and upper bound" in {
      val xsdTimeRangeRule = XsdTimeRangeRule("01:10:01", "01:20:10")
      xsdTimeRangeRule.evaluate(0, Row(List(Cell("01:15:01")), 1), Schema(globalDirsOne, List(ColumnDefinition("column1")))) mustEqual Success(true)
    }

    "pass if cell is equal to upper bound" in {
      val xsdTimeRangeRule = XsdTimeRangeRule("01:10:01", "01:10:10")
      xsdTimeRangeRule.evaluate(0, Row(List(Cell("01:10:10")), 1), Schema(globalDirsOne, List(ColumnDefinition("column1")))) mustEqual Success(true)
    }

    "succeed if cell just below upper bound" in {
      val xsdTimeRangeRule = XsdTimeRangeRule("01:10:01", "01:10:10")
      xsdTimeRangeRule.evaluate(0, Row(List(Cell("01:10:09")), 1), Schema(globalDirsOne, List(ColumnDefinition("column1")))) mustEqual Success(true)
    }

    "fail if cell is just below lower bound" in {
      val xsdTimeRangeRule = XsdTimeRangeRule("01:10:01", "01:10:10")
      xsdTimeRangeRule.evaluate(0, Row(List(Cell("01:10:00")), 1), Schema(globalDirsOne, List(ColumnDefinition("column1")))) must beLike {
        case Failure(messages) => messages.head mustEqual """xTime("01:10:01, 01:10:10") fails for line: 1, column: column1, value: "01:10:00""""
      }
    }
  }

}
