package uk.gov.tna.dri.schema

import org.specs2.mutable.Specification
import scalaz.{Failure, Success}
import uk.gov.tna.dri.metadata.{Cell, Row}

class FileExistsRuleSpec extends Specification{

  val emptyPathSubstitutions = List[(String,String)]()

  "FileExistsRule" should {

    val globalDirsOne = List(TotalColumns(1))
    val globalDirsTwo = List(TotalColumns(2))

    "fail for non-existent file" in {
      FileExistsRule(emptyPathSubstitutions).evaluate(0, Row(List(Cell("some/non/existent/file")), 1), Schema(globalDirsOne, List(ColumnDefinition("column1")))) must beLike {
        case Failure(messages) => messages.head mustEqual "fileExists fails for line: 1, column: column1, value: some/non/existent/file"
      }
    }

    "fail for empty file path" in {
      FileExistsRule(emptyPathSubstitutions).evaluate(1, Row(List(Cell("abc"), Cell("")), 2), Schema(globalDirsTwo, List(ColumnDefinition("column1"), ColumnDefinition("column2")))) must beLike {
        case Failure(messages) => messages.head mustEqual "fileExists fails for line: 2, column: column2, value: "
      }
    }

    "succeed for file that exists with no root file path" in {
      FileExistsRule(emptyPathSubstitutions).evaluate(0, Row(List(Cell("src/test/resources/uk/gov/tna/dri/schema/mustExistForRule.txt")), 1), Schema(globalDirsOne, List(ColumnDefinition("column1")))) must be_==(Success(true))
    }

    "succeed for file that exists with root file path" in {
      FileExistsRule(emptyPathSubstitutions,Literal(Some("src/test/resources/uk/gov/tna/"))).evaluate(0, Row(List(Cell("dri/schema/mustExistForRule.txt")), 1), Schema(globalDirsOne, List(ColumnDefinition("column1")))) must be_==(Success(true))
    }

    "succeed for root file path without final file seperator and file without initial file separator" in {
      FileExistsRule(emptyPathSubstitutions, Literal(Some("src/test/resources/uk/gov/tna"))).evaluate(0, Row(List(Cell("dri/schema/mustExistForRule.txt")), 1), Schema(globalDirsOne, List(ColumnDefinition("column1")))) must be_==(Success(true))
    }
  }


  "File system translation" should {
    val emptyPathSubstitutions =  List[(String,String)]()

    "succeed when checking that a file exists" in {
      new FileSystem(Some("src/test/resources/uk/gov/tna/"), "dri/schema/mustExistForRule.txt", emptyPathSubstitutions ).exists must beTrue
    }

    "succeed when checking that a file does NOT exist" in {
      new FileSystem(Some("src/test/resources/uk/gov/tna/"), "dri/schema/NOTAFILE.txt", emptyPathSubstitutions ).exists must beFalse
    }

    "succeed with help from substitutions to fix path" in {
      val pathSubstitutions =  List[(String,String)](
        ("bob", "src/test")
      )
      new FileSystem(Some("bob/resources/uk/gov/tna/"), "dri/schema/mustExistForRule.txt", pathSubstitutions ).exists must beTrue
    }

    "succeed with windows file seperators" in {
      new FileSystem(Some("""src\\test\\resources\\uk\\gov\\tna\\"""), """dri\\schema\\mustExistForRule.txt""", emptyPathSubstitutions ).exists must beTrue
    }

    "succeed even when the filename contains %20 spaces" in {
      val pathSubstitutions =  List[(String,String)](
        ("file://$ROOT", "file://" + System.getProperty("user.dir"))
      )
      new FileSystem(Some("file://$ROOT/src/test/resources/uk/gov/tna/"), "dri/schema/must%20Exist%20With%20Spaces%20For%20Rule.txt", pathSubstitutions ).exists must beTrue
    }
  }
}