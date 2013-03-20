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
        case Failure(messages) => messages.head mustEqual "fileExists fails for line: 1, column: column1, value: \"some/non/existent/file\""
      }
    }

    "fail for empty file path" in {
      FileExistsRule(emptyPathSubstitutions).evaluate(1, Row(List(Cell("abc"), Cell("")), 2), Schema(globalDirsTwo, List(ColumnDefinition("column1"), ColumnDefinition("column2")))) must beLike {
        case Failure(messages) => messages.head mustEqual "fileExists fails for line: 2, column: column2, value: \"\""
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
      FileSystem(Some("src/test/resources/uk/gov/tna/"), "dri/schema/mustExistForRule.txt", emptyPathSubstitutions ).exists must beTrue
    }

    "succeed when checking that a file does NOT exist" in {
      FileSystem(Some("src/test/resources/uk/gov/tna/"), "dri/schema/NOTAFILE.txt", emptyPathSubstitutions ).exists must beFalse
    }

    "succeed with help from substitutions to fix path" in {
      val pathSubstitutions =  List[(String,String)](
        ("bob", "src/test")
      )
      FileSystem(Some("bob/resources/uk/gov/tna/"), "dri/schema/mustExistForRule.txt", pathSubstitutions ).exists must beTrue
    }

    "succeed with windows file seperators" in {
      FileSystem(Some("""src\\test\\resources\\uk\\gov\\tna\\"""), """dri\\schema\\mustExistForRule.txt""", emptyPathSubstitutions ).exists must beTrue
    }

    "succeed even when the filename contains %20 spaces" in {
      val pathSubstitutions =  List[(String,String)](
        ("HOME", System.getProperty("user.dir"))
      )
      FileSystem(Some("file://HOME/src/test/resources/uk/gov/tna/"), "dri/schema/must%20Exist%20With%20Spaces%20For%20Rule.txt", pathSubstitutions ).exists must beTrue
    }

    "succeed when joining strings with missing '/'" in {
      val f = FileSystem(Some("file://root"), "file.txt", emptyPathSubstitutions )
      f.jointPath mustEqual "file://root/file.txt"
    }

    "succeed when joining strings with both having '/'" in {
      val f = FileSystem(Some("file://root/"), "/file.txt", emptyPathSubstitutions )
      f.jointPath mustEqual "file://root/file.txt"
    }

    "succeed when joining strings with base only having '/'" in {
      val f = FileSystem(Some("file://root"), "/file.txt", emptyPathSubstitutions )
      f.jointPath mustEqual "file://root/file.txt"
    }

    "succeed when joining strings with file only having '/'" in {
      val f = FileSystem(Some("file://root/"), "file.txt", emptyPathSubstitutions )
      f.jointPath mustEqual "file://root/file.txt"
    }

    "succeed when joining strings with file only having '/'" in {
      val f = FileSystem(None, "file.txt", emptyPathSubstitutions )
      f.jointPath mustEqual "file.txt"
    }
  }


  "file system substatution" should {
    "succeed with no substatutions" in {
      val pathSubstitutions =  List[(String,String)](
        ("Q", "X")
      )
      val f = FileSystem(Some("file://a/b/c/d/e/"), "file.txt", pathSubstitutions )
      f.expandBasePath  mustEqual  "file://a/b/c/d/e/file.txt"
    }

    "succeed with first substatution replacement" in {
      val pathSubstitutions =  List[(String,String)](
        ("a", "Z"),
        ("Q", "X")
      )
      val f = FileSystem(Some("file://a/b/c/d/e/"), "file.txt", pathSubstitutions )
      f.expandBasePath  mustEqual  "file://Z/b/c/d/e/file.txt"

    }

    "succeed with first substatution replacement" in {
      val pathSubstitutions =  List[(String,String)](
        ("P", "Z"),
        ("c", "X")
      )
      val f = FileSystem(Some("file://a/b/c/d/e/"), "file.txt", pathSubstitutions )
      f.expandBasePath  mustEqual  "file://a/b/X/d/e/file.txt"
    }

    "succeed with only first substatution replacement" in {
      val pathSubstitutions =  List[(String,String)](
        ("a", "Z"),
        ("c", "X")
      )
      val f = FileSystem(Some("file://a/b/c/d/e/"), "file.txt", pathSubstitutions )
      f.expandBasePath  mustEqual  "file://Z/b/c/d/e/file.txt"
    }
  }
}