package uk.gov.tna.dri.schema

import org.specs2.mutable.Specification
import scalaz.{Failure, Success}
import uk.gov.tna.dri.metadata.{Cell, Row}

class FileExistsRuleSpec extends Specification{

  "FileExistsRule" should {

    "fail for non-existent file" in {
      FileExistsRule(None).execute(0, Row(List(Cell("some/non/existent/file")), 1), Schema(1, List(ColumnDefinition("column1")))) must beLike {
        case Failure(msgs) => msgs.head mustEqual "fileExistsRule: fails for line 1, column: column1, value: some/non/existent/file"
      }
    }

    "fail for empty file path" in {
      FileExistsRule(None).execute(1, Row(List(Cell("abc"), Cell("")), 2), Schema(2, List(ColumnDefinition("column1"), ColumnDefinition("column2")))) must beLike {
        case Failure(msgs) => msgs.head mustEqual "fileExistsRule: fails for line 2, column: column2, value: "
      }
    }

    "succeed for file that exists with no root file path" in {
      FileExistsRule(None).execute(0, Row(List(Cell("src/test/resources/uk/gov/tna/dri/schema/mustExistForRule.txt")), 1), Schema(1, List(ColumnDefinition("column1")))) must be_==(Success(true))
    }

    "succeed for file that exists with root file path" in {
      FileExistsRule(Some("src/test/resources/uk/gov/tna/")).execute(0, Row(List(Cell("dri/schema/mustExistForRule.txt")), 1), Schema(1, List(ColumnDefinition("column1")))) must be_==(Success(true))
    }

    "succeed for root file path without final file seperator and file without initial file separator" in {
      FileExistsRule(Some("src/test/resources/uk/gov/tna")).execute(0, Row(List(Cell("dri/schema/mustExistForRule.txt")), 1), Schema(1, List(ColumnDefinition("column1")))) must be_==(Success(true))
    }
  }
}