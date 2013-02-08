package uk.gov.tna.dri.validator

import org.specs2.mutable.Specification
import uk.gov.tna.dri.schema._
import java.io.StringReader
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
        case Failure(messages) => messages.list must contain ("regex: [a-c]* fails for line 1, column: second", "regex: [3-8]* fails for line 2, column: first", "regex: [a-c]* fails for line 2, column: second").only
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
        case Failure(messages) => messages.list must contain ("Expected @TotalColumns of 1 and found 2 on line 1", "Expected @TotalColumns of 1 and found 2 on line 2").only
      }
    }

    "fail for a single rule" in {
      val m = "c11,c12"
      val schema = Schema(2, List(ColumnDefinition("Col1", List(RegexRule("C11"r))), ColumnDefinition("Col2")))

      validate(new StringReader(m), schema) should beLike {
        case Failure(messages) => messages.head mustEqual "regex: C11 fails for line 1, column: Col1"
      }
    }

    "fail for rule when cell missing" in {
      val m = "1"
      val schema = Schema(2, List(ColumnDefinition("Col1"), ColumnDefinition("Col2", List(RegexRule("[0-9]"r)))))

      validate(new StringReader(m), schema) should beLike {
        case Failure(messages) => messages.list must contain ("Expected @TotalColumns of 2 and found 1 on line 1", "Missing value at line: 1, column: Col2").only
      }
    }

    "succeed for multiple rows with InRule as string literal" in {
      val columnDefinitions = ColumnDefinition("col1") :: ColumnDefinition("col2WithRule", List(RegexRule("[0-9a-z]*".r), InRule(LiteralTypeProvider("dog")))) :: Nil
      val schema = Schema(2, columnDefinitions)

      val metaData =
        """someData,345dog
           someMore,12dog"""

      validate(new StringReader(metaData), schema) must beLike { case Success(_) => ok }
    }

    "succeed for InRule as column reference" in {
      val columnDefinitions = ColumnDefinition("col1") :: ColumnDefinition("col2WithRule", List(RegexRule("\\w*"r), InRule(ColumnTypeProvider("$col1")))) :: Nil
      val schema = Schema(2, columnDefinitions)

      val metaData =
        """mustBeIn,blah_mustBeIn_blah
           |andMustBeIn,blah_andMustBeIn""".stripMargin

      validate(new StringReader(metaData), schema) must beLike { case Success(_) => ok }
    }

    "fail for InRule as column reference where case does not match" in {
      val columnDefinitions = ColumnDefinition("col1") :: ColumnDefinition("col2WithRule", List(RegexRule("\\w*"r), InRule(ColumnTypeProvider("$col1")))) :: Nil
      val schema = Schema(2, columnDefinitions)

      val metaData =
        """mustBeIn,blah_MUSTBEIN_blah
          |andMustBeIn,blah_andMustBeIn""".stripMargin

      validate(new StringReader(metaData), schema) should beLike {
        case Failure(messages) => messages.list must haveTheSameElementsAs(List("inRule: mustBeIn fails for line 1, column: col2WithRule, value: blah_MUSTBEIN_blah"))
      }
    }

    "succeed for InRule as column reference where case is ignored" in {
      val columnDefinitions = ColumnDefinition("col1") :: ColumnDefinition("col2WithRule", List(RegexRule("\\w*"r), InRule(ColumnTypeProvider("$col1"))), List(IgnoreCase())) :: Nil
      val schema = Schema(2, columnDefinitions)

      val metaData =
        """mustBeIn,blah_MUSTBEIN_blah
          |andMustBeIn,blah_andMustBeIn""".stripMargin

      validate(new StringReader(metaData), schema) must beLike { case Success(_) => ok }
    }

    "succeed when @Optional is given for an empty cell" in {

      val columnDefinitions = ColumnDefinition("Col1") ::  ColumnDefinition("Col2", List(RegexRule("[0-9]"r)), List(Optional())) :: ColumnDefinition("Col3") :: Nil
      val schema = Schema(3, columnDefinitions)
      val m = "1, , 3"

      validate(new StringReader(m), schema) must beLike { case Success(_) => ok }
    }

    "fail when @Optional is given for non empty cell with a failing rule" in {

      val columnDefinitions = ColumnDefinition("Col1") ::  ColumnDefinition("Col2", List(RegexRule("[0-9]"r)), List(Optional())) :: ColumnDefinition("Col3") :: Nil
      val schema = Schema(3, columnDefinitions)
      val m = "1, a, 3"

      validate(new StringReader(m), schema) should beLike {
        case Failure(messages) => messages.head mustEqual "regex: [0-9] fails for line 1, column: Col2"
      }
    }

    "pass for empty cell with @Optional but fail for empty cell without @Optional for the same rule" in {

      val columnDefinitions = ColumnDefinition("Col1") :: ColumnDefinition("Col2", List(RegexRule("[0-9]"r)), List(Optional())) :: ColumnDefinition("Col3", List(RegexRule("[0-9]"r))) :: ColumnDefinition("Col4") :: Nil
      val schema = Schema(4, columnDefinitions)

      val m =
        """1,,,4
          |1,a,,4""".stripMargin

      validate(new StringReader(m), schema) should beLike {
        case Failure(messages) => messages.list must contain ("regex: [0-9] fails for line 1, column: Col3", "regex: [0-9] fails for line 2, column: Col2", "regex: [0-9] fails for line 2, column: Col3")
      }
    }

    "ignore case of a given regex" in {
      val columnDefinitions = ColumnDefinition("1", List(RegexRule("[a-z]+"r)), List(IgnoreCase())) :: Nil
      val schema = Schema(1, columnDefinitions)
      val meta = """SCOOBY"""

      validate(new StringReader(meta), schema) must beLike { case Success(_) => ok }
    }

    "fail to ignore case of a given regex when not providing @IgnoreCase" in {
      val columnDefinitions = ColumnDefinition("1", List(RegexRule("[a-z]+"r))) :: Nil
      val schema = Schema(1, columnDefinitions)
      val meta = """SCOOBY"""

      validate(new StringReader(meta), schema) should beLike {
        case Failure(messages) => messages.list must haveTheSameElementsAs(List("regex: [a-z]+ fails for line 1, column: 1"))
      }
    }

    "succeed with valid file path" in {
      val columnDefinitions = ColumnDefinition("1", List(FileExistsRule())) :: Nil
      val schema = Schema(1, columnDefinitions)
      val meta = "src/test/resources/uk/gov/tna/dri/schema/mustExistForRule.txt"

      validate(new StringReader(meta), schema) must beLike { case Success(_) => ok }
    }

    "succeed with valid file path where fileExists rule prepends root path to the filename" in {
      val columnDefinitions = ColumnDefinition("1", List(FileExistsRule(Some("src/test/resources/uk/gov/")))) :: Nil
      val schema = Schema(1, columnDefinitions)
      val meta = "tna/dri/schema/mustExistForRule.txt"

      validate(new StringReader(meta), schema) must beLike { case Success(_) => ok }
    }

    "fail for non existent file path" in {
      val columnDefinitions = ColumnDefinition("First Column", List(FileExistsRule())) :: Nil
      val schema = Schema(1, columnDefinitions)
      val meta = "some/non/existent/file"

      validate(new StringReader(meta), schema) must beLike {
        case Failure(messages) => messages.head mustEqual "fileExistsRule: fails for line 1, column: First Column, value: some/non/existent/file"
      }
    }
  }
}