package uk.gov.tna.dri.validator

import org.specs2.mutable.Specification
import uk.gov.tna.dri.schema.{ColumnDefinition, RegexRule, Schema}
import java.io.StringReader
import scalaz._

class MetaDataValidatorSpec extends Specification {

  object TestMetaDataValidator extends MetaDataValidator

  import TestMetaDataValidator._

  "Validation" should {

    "succeed for correct total columns for multiple lines" in {
      val metaData =
        """col1, col2
           col1, col2"""
      val colDefs = List(new ColumnDefinition("\"column1\""),new ColumnDefinition("\"column2\""))
      validate(new StringReader(metaData), Schema(2, colDefs)) must beLike { case Success(_) => ok }
    }

    "fail for incorrect number of total columns for multiple lines" in {
      val metaData =
        """col1, col2, col3
           col1, col2
           col1, col2, col3"""
      val colDefs = List(new ColumnDefinition("\"column1\""),new ColumnDefinition("\"column2\""),new ColumnDefinition("\"column3\""))
      validate(new StringReader(metaData), Schema(3, colDefs)) must beLike { case Failure(messages) => messages.head mustEqual "Expected @TotalColumns of 3 and found 2 on line 2" }
    }

    "fail if first column doesnt pass regex rule" in {

      val columnDefs = List(ColumnDefinition("first", List(RegexRule("[3-8]*".r))), ColumnDefinition("second"))
      validateRows(List(List("99", "xxx")), Schema(2, columnDefs)) must beLike { case Failure(msgs) => msgs.head mustEqual "regex: [3-8]* fails for line 1, column: first" }
    }


    "fail if columns on multiple rows do not pass" in {

      val schema = Schema(2, List(ColumnDefinition("first", List(RegexRule("[3-8]*".r))), ColumnDefinition("second",List(RegexRule("[a-c]*".r)))))
      val metaData =
        """34,xxxy
           abcd,uii"""

      validate(new StringReader(metaData), schema) must beLike {
        case Failure(msgs) => msgs.list must contain ("regex: [a-c]* fails for line 1, column: second", "regex: [3-8]* fails for line 2, column: first", "regex: [a-c]* fails for line 2, column: second").only
      }
    }

    "succeed if validates multiple rows" in {

      val rows = List(List("a", "1"), List("b", "2"))
      val schema = Schema(2, List(ColumnDefinition("column1", List(RegexRule("[a-c]".r))), ColumnDefinition("column2", List(RegexRule("[0-9]".r)))))
      validateRows(rows, schema) must beLike { case Success(_) => ok }
    }

    "succeed for valid schema and meta-data" in {

      val schema = Schema(2, List(ColumnDefinition("col1"), ColumnDefinition("col2WithRule", List(RegexRule("[0-9]*".r)))))
      val metaData =
        """someData,345
           someMore,12"""

      validate(new StringReader(metaData), schema) must beLike { case Success(_) => ok }
    }
  }
}
