package uk.gov.tna.dri.schema

import org.specs2.mutable.Specification
import scalax.file.{PathSet, Path}
import uk.gov.tna.dri.metadata.{Cell, Row}
import scalaz.{Failure, Scalaz, Success}

class FileCountSpec extends Specification {

  "File wildcards selection" should {

    "count multiple files in single directory using wildcards" in {
      val rootPath = Path.fromString("src/test/resources/uk/gov/tna/dri/fileCountTestFiles/threeFiles/")
      val scalaFiles = rootPath.matcher("**/*.jp2")

      rootPath.descendants().collect{ case s @ scalaFiles(_) => s }.size mustEqual 3
    }

    "count multiple files in subdirectories using wildcards" in {
      val rootPath = Path.fromString("src/test/resources/uk/gov/tna/dri/fileCountTestFiles/threeFilesinSubDir/")
      val scalaFiles = rootPath.matcher("**/*.jp2")

      rootPath.descendants().collect{ case s @ scalaFiles(_) => s }.size mustEqual 3
    }

    "count multiple files in subdirectories, using wildcards" in {
      val rootPath = Path.fromString("src/test/resources/uk/gov/tna/dri/fileCountTestFiles/threeFilesinSubDir/")
      val scalaFiles = rootPath.matcher("**/file*a.jp2")

      rootPath.descendants().collect{ case s @ scalaFiles(_) => s }.size mustEqual 3
    }

    "count files without looking in subdirectories, NO wildcards" in {
      val rootPath = Path.fromString("src/test/resources/uk/gov/tna/dri/fileCountTestFiles/threeFilesinSubDir/")
      val scalaFiles = rootPath.matcher("**/file1a.jp2")

      rootPath.descendants().collect{ case s @ scalaFiles(_) => s }.size mustEqual 1
    }
  }

  "fileCount" should {
    "find a match on a directory" in {
      val fileCountRule = new FileCountRule( Literal(Some("src/test/resources/uk/gov/tna/dri/fileCountTestFiles/threeFiles/**/*.jp2")) )
      fileCountRule.evaluate(0, Row(List(Cell("3")), 1), Schema(List(TotalColumns(1), NoHeader()), List(ColumnDefinition("column1")))) mustEqual Success(true)

    }

//    "find a match on a directory with basePath and file" in {
//      val fileCountRule = new FileCountRule( Literal(Some("src/test/resources/uk/gov/tna/dri/fileCountTestFiles/threeFiles/")), "**/*.jp2" )
//      fileCountRule.evaluate(0, Row(List(Cell("3")), 1), Schema(List(TotalColumns(1), NoHeader()), List(ColumnDefinition("column1")))) mustEqual Success(true)
//
//    }
  }

//  "fileCount with path substitutions" should {
//    val pathSubstitutions =  List[(String,String)](
//      ("bob", "src/test")
//    )
//
//    val fileCountRule = new FileCountRule(Literal(Some("""bob/resources/uk/gov/tna/dri/schema/checksum.txt""")), pathSubstitutions)
//    fileCountRule.evaluate(0, Row(List(Cell("3")), 1), Schema(List(TotalColumns(1), NoHeader()), List(ColumnDefinition("column1")))) mustEqual Success(true)
//  }



  "wildcard without substitutions " should {
    import scalaz.Scalaz._

    val wildCard = new FileWildcardSearch[Int]{
      val pathSubstitutions: List[(String, String)] = List.empty
      def matchWildcardPaths(matchList: PathSet[Path], fullPath: String): Scalaz.ValidationNEL[String, Int] = matchList.size.successNel[String]
      def matchSimplePath(fullPath: String): Scalaz.ValidationNEL[String, Int] = 1.successNel[String]
    }

    "find a single file from relative path" in {
      wildCard.search( ("src/test/resources/uk/gov/tna/dri/fileCountTestFiles/threeFiles/","file1.jp2") )   mustEqual Success(1)
    }

    "find a single file from relative windows path" in {
      wildCard.search( ("src\\test\\resources\\uk\\gov\\tna\\dri\\fileCountTestFiles\\threeFiles\\","file1.jp2") )   mustEqual Success(1)
    }

    "fail if an invalid relavtive basePath is given" in {
      wildCard.search( ("WRONGPATH/dri/fileCountTestFiles/threeFiles/","file1.jp2") ) must beLike {
        case Failure(m) => m.list mustEqual List("""incorrect root WRONGPATH/dri/fileCountTestFiles/threeFiles/ found""")
      }
    }

    "fail if invalid filename is given" in {
      wildCard.search( ("src/test/resources/uk/gov/tna/dri/fileCountTestFiles/threeFiles/","WRONG.WRONG") ) must beLike {
        case Failure(m) => m.list mustEqual List("""file "src/test/resources/uk/gov/tna/dri/fileCountTestFiles/threeFiles/WRONG.WRONG" not found""")
      }
    }

    "find multiple file fina single directory from relative path" in {
      wildCard.search( ("src/test/resources/uk/gov/tna/dri/fileCountTestFiles/threeFiles/","*.jp2") )   mustEqual Success(3)
    }

    "find multiple file in a single directory from relative path" in {
      wildCard.search( ("src/test/resources/uk/gov/tna/dri/fileCountTestFiles/threeFiles/","*.jp2") )   mustEqual Success(3)
    }

    "find multiple file in multi directories from relative path" in {
      wildCard.search( ("src/test/resources/uk/gov/tna/dri/fileCountTestFiles/threeFilesinSubDir/","**/*.jp2") )   mustEqual Success(3)
    }

    "find multiple file in multi directories from relative windows path" in {
      wildCard.search( ("src\\test\\resources\\uk\\gov\\tna\\dri\\fileCountTestFiles\\threeFilesinSubDir\\","**\\*.jp2") )   mustEqual Success(3)
    }
  }

  "wildcard with substitutions " should {
    import scalaz.Scalaz._

    val wildCard = new FileWildcardSearch[Int]{
      val pathSubstitutions: List[(String, String)] = List[(String,String)](
        ("bob", "src/test")
      )
      def matchWildcardPaths(matchList: PathSet[Path], fullPath: String): Scalaz.ValidationNEL[String, Int] = matchList.size.successNel[String]
      def matchSimplePath(fullPath: String): Scalaz.ValidationNEL[String, Int] = 1.successNel[String]
    }

    "find a single file from relative path" in {
      wildCard.search( ("bob/resources/uk/gov/tna/dri/fileCountTestFiles/threeFiles/","file1.jp2") )   mustEqual Success(1)
    }

    "find a single file from relative path" in {
      skipped( "needs a full path to test so will need to change the file://.... to test on local machine")

      val wildCard = new FileWildcardSearch[Int]{
        val pathSubstitutions: List[(String, String)] = List[(String,String)](
          ("file://bob", "file:///home/dev/IdeaProjects/csv/csv-validator/csv-validator-core/src/test")
        )
        def matchWildcardPaths(matchList: PathSet[Path], fullPath: String): Scalaz.ValidationNEL[String, Int] = matchList.size.successNel[String]
        def matchSimplePath(fullPath: String): Scalaz.ValidationNEL[String, Int] = 1.successNel[String]
      }

      wildCard.search( ("file://bob/resources/uk/gov/tna/dri/fileCountTestFiles/threeFiles/","file1.jp2") )   mustEqual Success(1)
    }

    "find a single file from relative windows path" in {
      wildCard.search( ("src\\test\\resources\\uk\\gov\\tna\\dri\\fileCountTestFiles\\threeFiles\\","file1.jp2") )   mustEqual Success(1)
    }

    "fail if an invalid relavtive basePath is given" in {
      wildCard.search( ("WRONGPATH/dri/fileCountTestFiles/threeFiles/","file1.jp2") ) must beLike {
        case Failure(m) => m.list mustEqual List("""incorrect root WRONGPATH/dri/fileCountTestFiles/threeFiles/ found""")
      }
    }

    "fail if an invalid relavtive basePath is given" in {
      wildCard.search( ("src/test/dri/fileCountTestFiles/threeFiles/","bob.jp2") ) must beLike {
        case Failure(m) => m.list mustEqual List("""incorrect root src/test/dri/fileCountTestFiles/threeFiles/ found""")
      }
    }

    "fail if invalid filename is given" in {
      wildCard.search( ("bob/resources/uk/gov/tna/dri/fileCountTestFiles/threeFiles/","WRONG.WRONG") ) must beLike {
        case Failure(m) => m.list mustEqual List("""file "src/test/resources/uk/gov/tna/dri/fileCountTestFiles/threeFiles/WRONG.WRONG" not found""")
      }
    }

    "find multiple file fina single directory from relative path" in {
      wildCard.search( ("bob/resources/uk/gov/tna/dri/fileCountTestFiles/threeFiles/","*.jp2") )   mustEqual Success(3)
    }

    "find multiple file in a single directory from relative path" in {
      wildCard.search( ("bob/resources/uk/gov/tna/dri/fileCountTestFiles/threeFiles/","*.jp2") )   mustEqual Success(3)
    }

    "find multiple file in multi directories from relative path" in {
      wildCard.search( ("bob/resources/uk/gov/tna/dri/fileCountTestFiles/threeFilesinSubDir/","**/*.jp2") )   mustEqual Success(3)
    }

    "find multiple file in multi directories from relative windows path" in {
      wildCard.search( ("src\\test\\resources\\uk\\gov\\tna\\dri\\fileCountTestFiles\\threeFilesinSubDir\\","**\\*.jp2") )   mustEqual Success(3)
    }
  }

}