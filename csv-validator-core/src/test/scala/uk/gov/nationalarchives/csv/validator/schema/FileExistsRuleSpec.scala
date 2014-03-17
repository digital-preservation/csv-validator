/**
 * Copyright (c) 2013, The National Archives <digitalpreservation@nationalarchives.gov.uk>
 * http://www.nationalarchives.gov.uk
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package uk.gov.nationalarchives.csv.validator.schema

import org.specs2.mutable.Specification
import scalaz.{Failure, Success}
import uk.gov.nationalarchives.csv.validator.metadata.{Cell, Row}
import uk.gov.nationalarchives.csv.validator.api.CsvValidator.SubstitutePath
import uk.gov.nationalarchives.csv.validator.TestResources
import uk.gov.nationalarchives.csv.validator.FILE_SEPARATOR
import uk.gov.nationalarchives.csv.validator.Util.FileSystem

class FileExistsRuleSpec extends Specification with TestResources {

  val relMustExistForRulePath = relResourcePath("mustExistForRule.csvs")

  val segments = relMustExistForRulePath.split(FILE_SEPARATOR)
  val relPath = (segments.slice(0, segments.length - 3).reduceLeft(_ + FILE_SEPARATOR + _), segments.slice(segments.length - 3, segments.length).reduceLeft(_ + FILE_SEPARATOR + _))

  val emptyPathSubstitutions = List[SubstitutePath]()

  "FileExistsRule" should {

    val globalDirsOne = List(TotalColumns(1))
    val globalDirsTwo = List(TotalColumns(2))

    "fail for non-existent file" in {
      FileExistsRule(emptyPathSubstitutions, false).evaluate(0, Row(List(Cell("some/non/existent/file")), 1), Schema(globalDirsOne, List(ColumnDefinition("column1")))) must beLike {
        case Failure(messages) => messages.head mustEqual "fileExists fails for line: 1, column: column1, value: \"some/non/existent/file\""
      }
    }

    "fail for empty file path" in {
      FileExistsRule(emptyPathSubstitutions, false).evaluate(1, Row(List(Cell("abc"), Cell("")), 2), Schema(globalDirsTwo, List(ColumnDefinition("column1"), ColumnDefinition("column2")))) must beLike {
        case Failure(messages) => messages.head mustEqual "fileExists fails for line: 2, column: column2, value: \"\""
      }
    }

    "succeed for file that exists with no root file path" in {
      FileExistsRule(emptyPathSubstitutions, false).evaluate(0, Row(List(Cell(relMustExistForRulePath)), 1), Schema(globalDirsOne, List(ColumnDefinition("column1")))) must be_==(Success(true))
    }

    "succeed for file that exists with root file path" in {
      FileExistsRule(emptyPathSubstitutions, false, Literal(Some(relPath._1 + FILE_SEPARATOR))).evaluate(0, Row(List(Cell(relPath._2)), 1), Schema(globalDirsOne, List(ColumnDefinition("column1")))) must be_==(Success(true))
    }

    "succeed for root file path without final file separator and file without initial file separator" in {
      FileExistsRule(emptyPathSubstitutions,false, Literal(Some(relPath._1))).evaluate(0, Row(List(Cell(relPath._2)), 1), Schema(globalDirsOne, List(ColumnDefinition("column1")))) must be_==(Success(true))
    }
  }

  "File system translation" should {
    val emptyPathSubstitutions =  List[(String,String)]()

    "succeed when checking that a file exists" in {
      FileSystem(Some(relPath._1 + FILE_SEPARATOR), relPath._2, emptyPathSubstitutions ).exists() must beTrue
    }

    "succeed when checking that a file does NOT exist" in {
      FileSystem(Some(relPath._1 + FILE_SEPARATOR), "dri/schema/NOTAFILE.csvs", emptyPathSubstitutions ).exists() must beFalse
    }

    "succeed with help from substitutions to fix path" in {
      val pathSubstitutions =  List[(String,String)](
        ("bob", relBaseResourcePkgPath)
      )
      FileSystem(Some("bob/"), "mustExistForRule.csvs", pathSubstitutions ).exists() must beTrue
    }

    "succeed with windows file separators" in {
      FileSystem(Some(relPath._1.replace('/', '\\')), relPath._2.replace('/', '\\'), emptyPathSubstitutions ).exists() must beTrue
    }

    "succeed even when the filename contains %20 spaces" in {

      val substitute =
        if(sys.props("os.name").toLowerCase.startsWith("win"))
          "HOME"
        else
          "/HOME"

      val pathSubstitutions =  List[(String,String)](
        (substitute, baseResourcePkgPath)
      )

      FileSystem(Some("file:///HOME/"), "must%20Exist%20With%20Spaces%20For%20Rule.csvs", pathSubstitutions ).exists() must beTrue
    }

  "succeed when the filename contains %20 spaces and the path is not a URI and already complete" in {

      val pathSubstitutions =  List[(String,String)](
      )

      FileSystem(None, baseResourcePkgPath + "/must%20Exist%20With%20Spaces%20For%20Rule.csvs", pathSubstitutions ).exists() must beTrue
    }

    "succeed when joining strings with missing '/'" in {
      val f = FileSystem(Some("file:///root"), "file.csvs", emptyPathSubstitutions )
      f.jointPath mustEqual "file:///root/file.csvs"
    }

    "succeed when joining strings with both having '/'" in {
      val f = FileSystem(Some("file:///root/"), "/file.csvs", emptyPathSubstitutions )
      f.jointPath mustEqual "file:///root/file.csvs"
    }

    "succeed when joining strings with base only having '/'" in {
      val f = FileSystem(Some("file:///root"), "/file.csvs", emptyPathSubstitutions )
      f.jointPath mustEqual "file:///root/file.csvs"
    }

    "succeed when joining strings with file only having '/'" in {
      val f = FileSystem(Some("file:///root/"), "file.csvs", emptyPathSubstitutions )
      f.jointPath mustEqual "file:///root/file.csvs"
    }

    "succeed when joining strings with file only having '/'" in {
      val f = FileSystem(None, "file.csvs", emptyPathSubstitutions )
      f.jointPath mustEqual "file.csvs"
    }
  }


  "file system substitution" should {
    "succeed with no substitutions" in {
      val pathSubstitutions =  List[(String,String)](
        ("Q", "X")
      )
      val f = FileSystem(Some("file:///a/b/c/d/e/"), "file.csvs", pathSubstitutions )
      f.expandBasePath  mustEqual  "file:///a/b/c/d/e/file.csvs"
    }

    "succeed with first substitution replacement" in {
      val pathSubstitutions =  List[(String,String)](
        ("a", "Z"),
        ("Q", "X")
      )
      val f = FileSystem(Some("file:///a/b/c/d/e/"), "file.csvs", pathSubstitutions )
      f.expandBasePath  mustEqual  "file:///Z/b/c/d/e/file.csvs"

    }

    "succeed with first substitution replacement" in {
      val pathSubstitutions =  List[(String,String)](
        ("P", "Z"),
        ("c", "X")
      )
      val f = FileSystem(Some("file:///a/b/c/d/e/"), "file.csvs", pathSubstitutions )
      f.expandBasePath  mustEqual  "file:///a/b/X/d/e/file.csvs"
    }

    "succeed with only first substitution replacement" in {
      val pathSubstitutions =  List[(String,String)](
        ("a", "Z"),
        ("c", "X")
      )
      val f = FileSystem(Some("file:///a/b/c/d/e/"), "file.csvs", pathSubstitutions )
      f.expandBasePath  mustEqual  "file:///Z/b/c/d/e/file.csvs"
    }
  }
}
