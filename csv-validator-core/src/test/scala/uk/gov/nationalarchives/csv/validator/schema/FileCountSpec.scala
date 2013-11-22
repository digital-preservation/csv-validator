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
import scalax.file.{PathSet, Path}
import uk.gov.nationalarchives.csv.validator.metadata.{Cell, Row}
import scalaz.{ValidationNel, Failure, Scalaz, Success}
import uk.gov.nationalarchives.csv.validator.TestResources
import uk.gov.nationalarchives.csv.validator.FILE_SEPARATOR
import java.io.File

class FileCountSpec extends Specification with TestResources {

  override val threeFilesPath = new File(new File(baseResourcePkgPath).getParentFile, "fileCountTestFiles/threeFiles").getAbsolutePath
  override val threeFilesInSubDirPath = new File(new File(baseResourcePkgPath).getParentFile, "fileCountTestFiles/threeFilesinSubDir").getAbsolutePath

  val relThreeFilesPath = relResourcePath("../fileCountTestFiles/threeFiles")
  val relThreeFilesInSubDirPath = relResourcePath("../fileCountTestFiles/threeFilesinSubDir")

  "File wildcards selection" should {

    "count multiple files in single directory using wildcards" in {
      val rootPath = Path.fromString(threeFilesPath)
      val scalaFiles = rootPath.matcher("**/*.jp2")

      rootPath.descendants().collect{ case s @ scalaFiles(_) => s }.size mustEqual 3
    }

    "count multiple files in subdirectories using wildcards" in {
      val rootPath = Path.fromString(threeFilesInSubDirPath)
      val scalaFiles = rootPath.matcher("**/*.jp2")

      rootPath.descendants().collect{ case s @ scalaFiles(_) => s }.size mustEqual 3
    }

    "count multiple files in subdirectories, using wildcards" in {
      val rootPath = Path.fromString(threeFilesInSubDirPath)
      val scalaFiles = rootPath.matcher("**/file*a.jp2")

      rootPath.descendants().collect{ case s @ scalaFiles(_) => s }.size mustEqual 3
    }

    "count files without looking in subdirectories, NO wildcards" in {
      val rootPath = Path.fromString(threeFilesInSubDirPath)
      val scalaFiles = rootPath.matcher("**/file1a.jp2")

      rootPath.descendants().collect{ case s @ scalaFiles(_) => s }.size mustEqual 1
    }
  }

  "fileCount" should {
    "find a match on a directory" in {
      val fileCountRule = new FileCountRule( Literal(Some(s"$threeFilesPath/**/*.jp2")) )
      val expectedFileCount = "3"
      fileCountRule.evaluate(0, Row(List(Cell(expectedFileCount)), 1), Schema(List(TotalColumns(1), NoHeader()), List(ColumnDefinition("column1")))) mustEqual Success(true)

    }

    "find a match on a directory with basePath and file" in {
      val fileCountRule = new FileCountRule( Literal(Some(s"$threeFilesPath/")), Literal(Some("**/*.jp2")) )
      val expectedFileCount: String = "3"
      fileCountRule.evaluate(0, Row(List(Cell(expectedFileCount)), 1), Schema(List(TotalColumns(1), NoHeader()), List(ColumnDefinition("column1")))) mustEqual Success(true)

    }
  }

  "fileCount with path substitutions" in {
    val pathSubstitutions =  List[(String,String)](
      ("bob", baseResourcePkgPath)
    )

    val fileCountRule = new FileCountRule(Literal(Some("""bob/checksum.txt""")), pathSubstitutions)
    val expectedFileCount = "1"
    fileCountRule.evaluate(0, Row(List(Cell(expectedFileCount)), 1), Schema(List(TotalColumns(1), NoHeader()), List(ColumnDefinition("column1")))) mustEqual Success(true)
  }



  "wildcard without substitutions " should {
    import scalaz.Scalaz._

    val wildCard = new FileWildcardSearch[Int]{
      val pathSubstitutions: List[(String, String)] = List.empty
      def matchWildcardPaths(matchList: PathSet[Path], fullPath: String): ValidationNel[String, Int] = matchList.size.successNel[String]
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
        case Failure(m) => m.list mustEqual List("""incorrect basepath WRONGPATH/dri/fileCountTestFiles/threeFiles/ (localfile: WRONGPATH/dri/fileCountTestFiles/threeFiles/file1.jp2) found""")
      }
    }

    "fail if invalid filename is given" in {
      wildCard.search( (threeFilesPath + FILE_SEPARATOR,"WRONG.WRONG") ) must beLike {
        case Failure(m) => m.list mustEqual List("""file """" + threeFilesPath + FILE_SEPARATOR + """WRONG.WRONG" not found""")
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
      def matchWildcardPaths(matchList: PathSet[Path], fullPath: String): ValidationNel[String, Int] = matchList.size.successNel[String]
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

        def matchWildcardPaths(matchList: PathSet[Path], fullPath: String): ValidationNel[String, Int] = matchList.size.successNel[String]
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
        case Failure(m) => m.list mustEqual List("""incorrect basepath WRONGPATH/dri/fileCountTestFiles/threeFiles/ (localfile: WRONGPATH/dri/fileCountTestFiles/threeFiles/file1.jp2) found""")
      }
    }

    "fail if an invalid relative basePath is given" in {
      wildCard.search( ("src/test/dri/fileCountTestFiles/threeFiles/","xfile.jp2") ) must beLike {
        case Failure(m) => m.list mustEqual List("""incorrect basepath src/test/dri/fileCountTestFiles/threeFiles/ (localfile: src/test/dri/fileCountTestFiles/threeFiles/xfile.jp2) found""")
      }
    }

    "fail if invalid filename is given" in {
      wildCard.search( ("bob/fileCountTestFiles/threeFiles/","WRONG.WRONG") ) must beLike {
        case Failure(m) => m.list mustEqual List("""file """" + Path.fromString(threeFilesPath).path + FILE_SEPARATOR + """WRONG.WRONG" not found""")
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