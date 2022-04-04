/*
 * Copyright (c) 2013, The National Archives <digitalpreservation@nationalarchives.gov.uk>
 * https://www.nationalarchives.gov.uk
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package uk.gov.nationalarchives.csv.validator.schema.v1_0

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import uk.gov.nationalarchives.csv.validator.{FILE_SEPARATOR, TestResources}
import uk.gov.nationalarchives.csv.validator.Util.TypedPath
import uk.gov.nationalarchives.csv.validator.metadata.{Cell, Row}
import uk.gov.nationalarchives.csv.validator.schema._
import scalaz.{Failure, IList, Success, ValidationNel}

import java.nio.file.Paths
import java.nio.file.Path

@RunWith(classOf[JUnitRunner])
class FileCountSpec extends Specification with TestResources {

  override val threeFilesPath = Paths.get(baseResourcePkgPath).getParent.getParent.resolve("fileCountTestFiles/threeFiles").toAbsolutePath.toString
  override val threeFilesInSubDirPath = Paths.get(baseResourcePkgPath).getParent.getParent.resolve("fileCountTestFiles/threeFilesinSubDir").toAbsolutePath.toString

  val relThreeFilesPath = relResourcePath("../../fileCountTestFiles/threeFiles")
  val relThreeFilesInSubDirPath = relResourcePath("../../fileCountTestFiles/threeFilesinSubDir")

  //TODO(AR) disabled these tests as they seem to be testing the scalax.file.Path library rather than CSV Validator code
//  "File wildcards selection" should {
//
//    "count multiple files in single directory using wildcards" in {
//      val rootPath = Paths.get(threeFilesPath)
//      val scalaFiles = rootPath.matcher("**/*.jp2")
//
//      rootPath.descendants().collect{ case s @ scalaFiles(_) => s }.size mustEqual 3
//    }
//
//    "count multiple files in subdirectories using wildcards" in {
//      val rootPath = Paths.get(threeFilesInSubDirPath)
//      val scalaFiles = rootPath.matcher("**/*.jp2")
//
//      rootPath.descendants().collect{ case s @ scalaFiles(_) => s }.size mustEqual 3
//    }
//
//    "count multiple files in subdirectories, using wildcards" in {
//      val rootPath = Paths.get(threeFilesInSubDirPath)
//      val scalaFiles = rootPath.matcher("**/file*a.jp2")
//
//      rootPath.descendants().collect{ case s @ scalaFiles(_) => s }.size mustEqual 3
//    }
//
//    "count files without looking in subdirectories, NO wildcards" in {
//      val rootPath = Paths.get(threeFilesInSubDirPath)
//      val scalaFiles = rootPath.matcher("**/file1a.jp2")
//
//      rootPath.descendants().collect{ case s @ scalaFiles(_) => s }.size mustEqual 1
//    }
//  }

  "fileCount" should {
    "find a match on a directory" in {
      val fileCountRule = new FileCountRule( Literal(Some(s"$threeFilesPath/**/*.jp2")), List.empty )
      val expectedFileCount = "3"
      fileCountRule.evaluate(0, Row(List(Cell(expectedFileCount)), 1), Schema(List(TotalColumns(1), NoHeader()), List(ColumnDefinition(NamedColumnIdentifier("column1"))))) mustEqual Success(true)

    }

    "find a match on a directory with basePath and file" in {
      val fileCountRule = new FileCountRule( Literal(Some(s"$threeFilesPath/")), Literal(Some("**/*.jp2")) )
      val expectedFileCount: String = "3"
      fileCountRule.evaluate(0, Row(List(Cell(expectedFileCount)), 1), Schema(List(TotalColumns(1), NoHeader()), List(ColumnDefinition(NamedColumnIdentifier("column1"))))) mustEqual Success(true)

    }
  }

  "fileCount with path substitutions" in {
    val pathSubstitutions =  List[(String,String)](
      ("bob", baseResourcePkgPath)
    )

    val fileCountRule = new FileCountRule(Literal(Some("""bob/checksum.csvs""")), pathSubstitutions)
    val expectedFileCount = "1"
    fileCountRule.evaluate(0, Row(List(Cell(expectedFileCount)), 1), Schema(List(TotalColumns(1), NoHeader()), List(ColumnDefinition(NamedColumnIdentifier("column1"))))) mustEqual Success(true)
  }



  "wildcard without substitutions " should {
    import scalaz.Scalaz._

    val wildCard = new FileWildcardSearch[Int]{
      val pathSubstitutions: List[(String, String)] = List.empty
      def matchWildcardPaths(matchList: Seq[Path], fullPath: String): ValidationNel[String, Int] = matchList.size.successNel[String]
      def matchSimplePath(fullPath: String): ValidationNel[String, Int] = 1.successNel[String]
    }

    "find a single file from relative path" in {
      wildCard.search( (relThreeFilesPath + FILE_SEPARATOR,"file1.jp2") )   mustEqual Success(1)
    }

    "find a single file from relative windows path" in {
      val windowsPath = relThreeFilesPath.replace('/', '\\')
      wildCard.search( (windowsPath + '\\' ,"file1.jp2") )   mustEqual Success(1)
    }

    "fail if an invalid relavtive basePath is given" in {
      wildCard.search( ("WRONGPATH/dri/fileCountTestFiles/threeFiles/","file1.jp2") ) must beLike {
        case Failure(m) => m.list mustEqual IList("""incorrect basepath WRONGPATH/dri/fileCountTestFiles/threeFiles/ (localfile: """ + TypedPath("WRONGPATH/dri/fileCountTestFiles/threeFiles/file1.jp2").toPlatform + """) found""")
      }
    }

    "fail if invalid filename is given" in {
      wildCard.search( (threeFilesPath + FILE_SEPARATOR,"WRONG.WRONG") ) must beLike {
        case Failure(m) => m.list mustEqual IList("""file """" + threeFilesPath + FILE_SEPARATOR + """WRONG.WRONG" not found""")
      }
    }

    "find multiple file in a single directory from relative path" in {
      wildCard.search( (relThreeFilesPath + FILE_SEPARATOR,"*.jp2") )   mustEqual Success(3)
    }

    "find multiple file in multi directories from relative path" in {
      wildCard.search( (relThreeFilesInSubDirPath + FILE_SEPARATOR,"**/*.jp2") )   mustEqual Success(3)
    }

    "find multiple file in multi directories from relative windows path" in {
      val windowsPath = relThreeFilesInSubDirPath.replace('/', '\\')
      wildCard.search( (windowsPath + '\\',"**\\*.jp2") )   mustEqual Success(3)
    }
  }

  "wildcard with substitutions " should {
    import scalaz.Scalaz._

    val wildCard = new FileWildcardSearch[Int]{
      val pathSubstitutions: List[(String, String)] = List[(String,String)](
        ("bob", basePath + "/uk/gov/nationalarchives/csv/validator")
      )
      def matchWildcardPaths(matchList: Seq[Path], fullPath: String): ValidationNel[String, Int] = matchList.size.successNel[String]
      def matchSimplePath(fullPath: String): ValidationNel[String, Int] = 1.successNel[String]
    }

    "find a single file from relative path" in {
      wildCard.search( ("bob/fileCountTestFiles/threeFiles/","file1.jp2") )   mustEqual Success(1)
    }

    "find a single file from relative path" in {

      val wildCard = new FileWildcardSearch[Int]{

        val substituted =
          if(sys.props("os.name").toLowerCase.startsWith("win"))
            s"file:///${basePath}/uk/gov/nationalarchives/csv/validator"
          else
            s"file://${basePath}/uk/gov/nationalarchives/csv/validator"

        val pathSubstitutions: List[(String, String)] = List[(String, String)](
          ("file:///bob", substituted)
        )

        def matchWildcardPaths(matchList: Seq[Path], fullPath: String): ValidationNel[String, Int] = matchList.size.successNel[String]
        def matchSimplePath(fullPath: String): ValidationNel[String, Int] = 1.successNel[String]
      }

      wildCard.search( ("file:///bob/fileCountTestFiles/threeFiles/","file1.jp2") )   mustEqual Success(1)
    }

    "find a single file from relative windows path" in {
      val windowsPath = relThreeFilesPath.replace('/', '\\')
      wildCard.search( (windowsPath + '\\',"file1.jp2") )   mustEqual Success(1)
    }

    "fail if an invalid relative basePath is given" in {
      wildCard.search( ("WRONGPATH/dri/fileCountTestFiles/threeFiles/","file1.jp2") ) must beLike {
        case Failure(m) => m.list mustEqual IList("""incorrect basepath WRONGPATH/dri/fileCountTestFiles/threeFiles/ (localfile: """ + TypedPath("WRONGPATH/dri/fileCountTestFiles/threeFiles/file1.jp2").toPlatform + """) found""")
      }
    }

    "fail if an invalid relative basePath is given" in {
      wildCard.search( ("src/test/dri/fileCountTestFiles/threeFiles/","xfile.jp2") ) must beLike {
        case Failure(m) => m.list mustEqual IList("""incorrect basepath src/test/dri/fileCountTestFiles/threeFiles/ (localfile: """ + TypedPath("src/test/dri/fileCountTestFiles/threeFiles/xfile.jp2").toPlatform + """) found""")
      }
    }

    "fail if invalid filename is given" in {
      wildCard.search( ("bob/fileCountTestFiles/threeFiles/","WRONG.WRONG") ) must beLike {
        case Failure(m) => m.list mustEqual IList("""file """" + Paths.get(threeFilesPath).toString + FILE_SEPARATOR + """WRONG.WRONG" not found""")
      }
    }

    "find multiple files in a single directory from relative path" in {
      wildCard.search( ("bob/fileCountTestFiles/threeFiles/","*.jp2") )   mustEqual Success(3)
    }

    "find multiple file in a single directory from relative path" in {
      wildCard.search( ("bob/fileCountTestFiles/threeFiles/","*.jp2") )   mustEqual Success(3)
    }

    "find multiple file in multi directories from relative path" in {
      wildCard.search( ("bob/fileCountTestFiles/threeFilesinSubDir/","**/*.jp2") )   mustEqual Success(3)
    }

    "find multiple file in multi directories from relative windows path" in {
      val windowsPath = relThreeFilesInSubDirPath.replace('/', '\\')
      wildCard.search( (windowsPath + '\\',"**\\*.jp2") )   mustEqual Success(3)
    }
  }

}