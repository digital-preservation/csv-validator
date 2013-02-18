package uk.gov.tna.dri.validator

import org.specs2.mutable.Specification
import uk.gov.tna.dri.schema._
import java.io.StringReader
import scalaz.Success
import uk.gov.tna.dri.schema.ColumnDefinition
import uk.gov.tna.dri.schema.RegexRule
import scalaz.Failure
import uk.gov.tna.dri.schema.Schema

class MetaDataValidatorSpec extends Specification {

  object TestMetaDataValidator extends AllErrorsMetaDataValidator

  import TestMetaDataValidator._

  "Validation" should {
    val globalDirsOne = GlobalDirectives(TotalColumnsDirective(1), Some(NoHeaderDirective()), None)
    val globalDirsTwo = GlobalDirectives(TotalColumnsDirective(2), Some(NoHeaderDirective()), None)
    val globalDirsThree = GlobalDirectives(TotalColumnsDirective(3), Some(NoHeaderDirective()), None)
    val globalDirsFour = GlobalDirectives(TotalColumnsDirective(4), Some(NoHeaderDirective()), None)

    "succeed for correct total columns for multiple lines" in {
      val metaData =
        """col1, col2
           col1, col2"""

      val columnDefinitions = List(new ColumnDefinition("\"column1\""),new ColumnDefinition("\"column2\""))
      validate(new StringReader(metaData), Schema(globalDirsTwo, columnDefinitions)) must beLike { case Success(_) => ok }
    }

    "fail for incorrect number of total columns for multiple lines" in {
      val metaData =
        """col1, col2, col3
           col1, col2
           col1, col2, col3"""

      val columnDefinitions = List(new ColumnDefinition("\"column1\""),new ColumnDefinition("\"column2\""),new ColumnDefinition("\"column3\""))
      validate(new StringReader(metaData), Schema(globalDirsThree, columnDefinitions)) must beLike {
        case Failure(messages) => messages.list mustEqual List(
          "Expected @TotalColumns of 3 and found 2 on line 2",
          "Missing value at line: 2, column: \"column3\"")
      }
    }

    "fail if columns on multiple rows do not pass" in {
      val schema = Schema(globalDirsTwo, List(ColumnDefinition("first", List(RegexRule(Literal(Some("[3-8]*"))))), ColumnDefinition("second",List(RegexRule(Literal(Some("[a-c]*")))))))

      val metaData =
        """34,xxxy
           |abcd,uii""".stripMargin

      validate(new StringReader(metaData), schema) must beLike {
        case Failure(messages) => messages.list mustEqual List (
          "regex: [a-c]* fails for line: 1, column: second, value: xxxy",
          "regex: [3-8]* fails for line: 2, column: first, value: abcd",
          "regex: [a-c]* fails for line: 2, column: second, value: uii")
      }
    }

    "succeed for multiple rows" in {
      val schema = Schema(globalDirsTwo, List(ColumnDefinition("col1"), ColumnDefinition("col2WithRule", List(RegexRule(Literal(Some("[0-9]*")))))))

      val metaData =
        """someData,345
           someMore,12"""

      validate(new StringReader(metaData), schema) must beLike { case Success(_) => ok }
    }

    "fail for @TotalColumns invalid" in {
      val m = """c11,c12
                |c21,c22""".stripMargin

      val schema = Schema(globalDirsOne, List(ColumnDefinition("Col1")))

      validate(new StringReader(m), schema) should beLike {
        case Failure(messages) => messages.list mustEqual List("Expected @TotalColumns of 1 and found 2 on line 1", "Expected @TotalColumns of 1 and found 2 on line 2")
      }
    }

    "fail for a single rule" in {
      val m = "c11,c12"
      val schema = Schema(globalDirsTwo, List(ColumnDefinition("Col1", List(RegexRule(Literal(Some("C11"))))), ColumnDefinition("Col2")))

      validate(new StringReader(m), schema) should beLike {
        case Failure(messages) => messages.list mustEqual List("regex: C11 fails for line: 1, column: Col1, value: c11")
      }
    }

    "fail for rule when cell missing" in {
      val m = "1"
      val schema = Schema(globalDirsTwo, List(ColumnDefinition("Col1"), ColumnDefinition("Col2", List(RegexRule(Literal(Some("[0-9]")))))))

      validate(new StringReader(m), schema) should beLike {
        case Failure(messages) => messages.list mustEqual List("Expected @TotalColumns of 2 and found 1 on line 1", "Missing value at line: 1, column: Col2")
      }
    }

    "succeed for more than one regex" in {
      val columnDefinitions = ColumnDefinition("c1", List(RegexRule(Literal(Some("\\w+"))), RegexRule(Literal(Some("^S.+"))))) :: Nil
      val schema = Schema(globalDirsOne, columnDefinitions)

      val metaData = """Scooby"""

      validate(new StringReader(metaData), schema) must beLike { case Success(_) => ok }
    }

    "fail when at least one regex fails for multiple regex provided in a column definition" in {
      val columnDefinitions = ColumnDefinition("c1", List(RegexRule(Literal(Some("\\w+"))), RegexRule(Literal(Some("^T.+"))), RegexRule(Literal(Some("^X.+"))))) :: Nil
      val schema = Schema(globalDirsOne, columnDefinitions)

      val metaData = """Scooby"""

      validate(new StringReader(metaData), schema) must beLike {
        case Failure(messages) => messages.list mustEqual List("regex: ^T.+ fails for line: 1, column: c1, value: Scooby", "regex: ^X.+ fails for line: 1, column: c1, value: Scooby")
      }
    }

    "succeed for multiple rows with InRule as string literal" in {
      val columnDefinitions = ColumnDefinition("col1") :: ColumnDefinition("col2WithRule", List(RegexRule(Literal(Some("[0-9a-z]*"))), InRule(Literal(Some("345dog"))))) :: Nil
      val schema = Schema(globalDirsTwo, columnDefinitions)

      val metaData =
        """someData,dog
           someMore,dog"""

      validate(new StringReader(metaData), schema) must beLike { case Success(_) => ok }
    }

    "succeed for InRule as column reference" in {
      val columnDefinitions = ColumnDefinition("col1") :: ColumnDefinition("col2WithRule", List(RegexRule(Literal(Some("\\w*"))), InRule(ColumnReference("col1")))) :: Nil
      val schema = Schema(globalDirsTwo, columnDefinitions)

      val metaData =
        """blah_mustBeIn_blah,mustBeIn
           blah_andMustBeIn,andMustBeIn"""

      validate(new StringReader(metaData), schema) must beLike { case Success(_) => ok }
    }

    "fail for InRule as column reference where case does not match" in {
      val columnDefinitions = ColumnDefinition("col1") :: ColumnDefinition("col2WithRule", List(RegexRule(Literal(Some("\\w*"))), InRule(ColumnReference("col1")))) :: Nil
      val schema = Schema(globalDirsTwo, columnDefinitions)

      val metaData =
        """blah_MUSTBEIN_blah,mustBeIn
           blah_andMustBeIn,andMustBeIn"""

      validate(new StringReader(metaData), schema) should beLike {
        case Failure(messages) => messages.list mustEqual List("in: blah_MUSTBEIN_blah fails for line: 1, column: col2WithRule, value: mustBeIn")
      }
    }

    "succeed for InRule as column reference where case is ignored" in {
      val columnDefinitions = ColumnDefinition("col1") :: ColumnDefinition("col2WithRule", List(RegexRule(Literal(Some("\\w*"))), InRule(ColumnReference("col1"))), List(IgnoreCase())) :: Nil
      val schema = Schema(globalDirsTwo, columnDefinitions)

      val metaData =
        """blah_MUSTBEIN_blah,mustBeIn
           blah_andMustBeIn,andMustBeIn"""

      validate(new StringReader(metaData), schema) must beLike { case Success(_) => ok }
    }

    "succeed when @Optional is given for an empty cell" in {

      val columnDefinitions = ColumnDefinition("Col1") ::  ColumnDefinition("Col2", List(RegexRule(Literal(Some("[0-9]")))), List(Optional())) :: ColumnDefinition("Col3") :: Nil
      val schema = Schema(globalDirsThree, columnDefinitions)
      val m = "1, , 3"

      validate(new StringReader(m), schema) must beLike { case Success(_) => ok }
    }

    "fail when @Optional is given for non empty cell with a failing rule" in {

      val columnDefinitions = ColumnDefinition("Col1") ::  ColumnDefinition("Col2", List(RegexRule(Literal(Some("[0-9]")))), List(Optional())) :: ColumnDefinition("Col3") :: Nil
      val schema = Schema(globalDirsThree, columnDefinitions)
      val m = "1,a,3"

      validate(new StringReader(m), schema) should beLike {
        case Failure(messages) => messages.list mustEqual List("regex: [0-9] fails for line: 1, column: Col2, value: a")
      }
    }

    "pass for empty cell with @Optional but fail for empty cell without @Optional for the same rule" in {

      val columnDefinitions = ColumnDefinition("Col1") :: ColumnDefinition("Col2", List(RegexRule(Literal(Some("[0-9]")))), List(Optional())) :: ColumnDefinition("Col3", List(RegexRule(Literal(Some("[0-9]"))))) :: ColumnDefinition("Col4") :: Nil
      val schema = Schema(globalDirsFour, columnDefinitions)

      val m =
        """1,,,4
          |1,a,,4""".stripMargin

      validate(new StringReader(m), schema) should beLike {
        case Failure(messages) => messages.list mustEqual List(
          "regex: [0-9] fails for line: 1, column: Col3, value: ",
          "regex: [0-9] fails for line: 2, column: Col2, value: a",
          "regex: [0-9] fails for line: 2, column: Col3, value: ")
      }
    }

    "ignore case of a given regex" in {
      val columnDefinitions = ColumnDefinition("1", List(RegexRule(Literal(Some("[a-z]+")))), List(IgnoreCase())) :: Nil
      val schema = Schema(globalDirsOne, columnDefinitions)
      val meta = """SCOOBY"""

      validate(new StringReader(meta), schema) must beLike { case Success(_) => ok }
    }

    "ignore case with in rule succeeds if value contains reg ex characters" in {
      val columnDefinitions = ColumnDefinition("1", List(InRule(Literal(Some("[abc]")))), List(IgnoreCase())) :: Nil
      val schema = Schema(globalDirsOne, columnDefinitions)
      val meta ="""[abc"""
      validate(new StringReader(meta), schema) must beLike { case Success(_) => ok }
    }

    "fail to ignore case of a given regex when not providing @IgnoreCase" in {
      val columnDefinitions = ColumnDefinition("Col1", List(RegexRule(Literal(Some("[a-z]+"))))) :: Nil
      val schema = Schema(globalDirsOne, columnDefinitions)
      val meta = "SCOOBY"

      validate(new StringReader(meta), schema) should beLike {
        case Failure(messages) => messages.list mustEqual List("regex: [a-z]+ fails for line: 1, column: Col1, value: SCOOBY")
      }
    }

    "succeed with valid file path" in {
      val columnDefinitions = ColumnDefinition("1", List(FileExistsRule(Literal(None)))) :: Nil
      val schema = Schema(globalDirsOne, columnDefinitions)
      val meta = "src/test/resources/uk/gov/tna/dri/schema/mustExistForRule.txt"

      validate(new StringReader(meta), schema) must beLike { case Success(_) => ok }
    }

    "succeed with valid file path where fileExists rule prepends root path to the filename" in {
      val columnDefinitions = ColumnDefinition("1", List(FileExistsRule(Literal(Some("src/test/resources/uk/gov/"))))) :: Nil
      val schema = Schema(globalDirsOne, columnDefinitions)
      val meta = "tna/dri/schema/mustExistForRule.txt"

      validate(new StringReader(meta), schema) must beLike { case Success(_) => ok }
    }

    "fail for non existent file path" in {
      val columnDefinitions = ColumnDefinition("First Column", List(FileExistsRule(Literal(None)))) :: Nil
      val schema = Schema(globalDirsOne, columnDefinitions)
      val meta = "some/non/existent/file"

      validate(new StringReader(meta), schema) must beLike {
        case Failure(messages) => messages.list mustEqual List("fileExists: fails for line: 1, column: First Column, value: some/non/existent/file")
      }
    }

    "fail when first line contains invalid data and noHeader directive is set" in {
      val columnDefinitions = ColumnDefinition("col1") :: ColumnDefinition("col2WithRule", List(RegexRule(Literal(Some("[0-9a-z]*"))), InRule(Literal(Some("345dog"))))) :: Nil
      val schema = Schema(globalDirsTwo, columnDefinitions)
      val metaData =
        """someData,thisisrubbish
           someMore,dog"""

      validate(new StringReader(metaData), schema) must beLike {
        case Failure(messages) => messages.list mustEqual List("in: 345dog fails for line: 1, column: col2WithRule, value: thisisrubbish")
      }
    }

    "succeed when first line contains invalid data and noHeader directive is missing" in {
      val columnDefinitions = ColumnDefinition("col1") :: ColumnDefinition("col2WithRule", List(RegexRule(Literal(Some("[0-9a-z]*"))), InRule(Literal(Some("345dog"))))) :: Nil
      val globalDirsTwoNoheader = GlobalDirectives(TotalColumnsDirective(2), None, None)
      val schema = Schema(globalDirsTwoNoheader, columnDefinitions)
      val metaData =
        """someData,thisisrubbish
           someMore,dog"""

      validate(new StringReader(metaData), schema) must beLike { case Success(_) => ok }
    }

  }
}


