package uk.gov.tna.dri.validator

import org.specs2.mutable.Specification
import uk.gov.tna.dri.schema._
import java.io.StringReader
import scalaz._
import scalaz.Success
import uk.gov.tna.dri.schema.ColumnDefinition
import uk.gov.tna.dri.schema.RegexRule
import scalaz.Failure
import uk.gov.tna.dri.schema.Schema
import uk.gov.tna.dri.schema.LiteralTypeProvider

class MetaDataValidatorSpec extends Specification {

  object TestMetaDataValidator extends MetaDataValidator

  import TestMetaDataValidator._

  "Validation" should {

    "succeed for correct total columns for multiple lines" in {
      val metaData =
        """col1, col2
           col1, col2"""

      val columnDefinitions = List(new ColumnDefinition("\"column1\""),new ColumnDefinition("\"column2\""))
      validate(new StringReader(metaData), Schema(2, columnDefinitions)) must beLike { case Success(_) => ok }
    }

    "fail for incorrect number of total columns for multiple lines" in {
      val metaData =
        """col1, col2, col3
           col1, col2
           col1, col2, col3"""

      val columnDefinitions = List(new ColumnDefinition("\"column1\""),new ColumnDefinition("\"column2\""),new ColumnDefinition("\"column3\""))
      validate(new StringReader(metaData), Schema(3, columnDefinitions)) must beLike { case Failure(messages) => messages.head mustEqual "Expected @TotalColumns of 3 and found 2 on line 2" }
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

    "succeed for multiple rows" in {
      val schema = Schema(2, List(ColumnDefinition("col1"), ColumnDefinition("col2WithRule", List(RegexRule("[0-9]*".r)))))

      val metaData =
        """someData,345
           someMore,12"""

      validate(new StringReader(metaData), schema) must beLike { case Success(_) => ok }
    }

    "fail for @TotalColumns invalid" in {
      val m = """c11,c12
                |c21,c22""".stripMargin

      val schema = Schema(1, List(ColumnDefinition("Col1")))

      validate(new StringReader(m), schema) should beLike {
        case Failure(msgs) => msgs.list must contain ("Expected @TotalColumns of 1 and found 2 on line 1", "Expected @TotalColumns of 1 and found 2 on line 2").only
      }
    }

    "fail for a single rule" in {
      val m = "c11,c12"
      val schema = Schema(2, List(ColumnDefinition("Col1", List(RegexRule("C11"r))), ColumnDefinition("Col2")))

      validate(new StringReader(m), schema) should beLike {
        case Failure(msgs) => msgs.head mustEqual "regex: C11 fails for line 1, column: Col1"
      }
    }

    "fail for rule when cell missing" in {
      val m = "1"
      val schema = Schema(2, List(ColumnDefinition("Col1"), ColumnDefinition("Col2", List(RegexRule("[0-9]"r)))))

      validate(new StringReader(m), schema) should beLike {
        case Failure(msgs) => msgs.list must contain ("Expected @TotalColumns of 2 and found 1 on line 1", "Missing value at line: 1, column: Col2").only
      }
    }

    "succeed for multiple rows with InRule" in {

      val schema = Schema(2, List(ColumnDefinition("col1"), ColumnDefinition("col2WithRule", List(RegexRule("[0-9a-z]*".r),InRule(LiteralTypeProvider("dog"))))))
      val metaData =
        """someData,345dog
           someMore,12dog"""

      validate(new StringReader(metaData), schema) must beLike { case Success(_) => ok }
    }

  }
}
