package uk.gov.tna.dri.schema

import org.specs2.mutable.Specification
import scalaz.{Failure, Success}
import uk.gov.tna.dri.metadata.{Cell, Row}

class FileExistsRuleSpec extends Specification{

  "FileExistsRule" should {

    val globalDirsOne = List(TotalColumns(1))
    val globalDirsTwo = List(TotalColumns(2))

    "fail for non-existent file" in {
      FileExistsRule(Literal(None)).evaluate(0, Row(List(Cell("some/non/existent/file")), 1), Schema(globalDirsOne, List(ColumnDefinition("column1")))) must beLike {
        case Failure(messages) => messages.head mustEqual "fileExists: fails for line: 1, column: column1, value: some/non/existent/file"
      }
    }

    "fail for empty file path" in {
      FileExistsRule(Literal(None)).evaluate(1, Row(List(Cell("abc"), Cell("")), 2), Schema(globalDirsTwo, List(ColumnDefinition("column1"), ColumnDefinition("column2")))) must beLike {
        case Failure(messages) => messages.head mustEqual "fileExists: fails for line: 2, column: column2, value: "
      }
    }

    "succeed for file that exists with no root file path" in {
      FileExistsRule(Literal(None)).evaluate(0, Row(List(Cell("src/test/resources/uk/gov/tna/dri/schema/mustExistForRule.txt")), 1), Schema(globalDirsOne, List(ColumnDefinition("column1")))) must be_==(Success(true))
    }

    "succeed for file that exists with root file path" in {
      FileExistsRule(Literal(Some("src/test/resources/uk/gov/tna/"))).evaluate(0, Row(List(Cell("dri/schema/mustExistForRule.txt")), 1), Schema(globalDirsOne, List(ColumnDefinition("column1")))) must be_==(Success(true))
    }

    "succeed for root file path without final file seperator and file without initial file separator" in {
      FileExistsRule(Literal(Some("src/test/resources/uk/gov/tna"))).evaluate(0, Row(List(Cell("dri/schema/mustExistForRule.txt")), 1), Schema(globalDirsOne, List(ColumnDefinition("column1")))) must be_==(Success(true))
    }
  }
}