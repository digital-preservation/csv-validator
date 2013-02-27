package uk.gov.tna.dri.schema

import org.specs2.mutable.Specification
import scalax.file.{PathMatcher, Path}


class FileCountSpec extends Specification {

  "File wildcards selection" should {

    "count multiple files in single directory using wildcards" in {
      val rootPath = Path.fromString("src/test/resources/uk/gov/tna/dri/fileCountTestFiles/threeFiles/")
      val scalaFiles = rootPath.matcher("**/*.jp2")

      rootPath.descendants().collect{ case s@scalaFiles(_) => s }.size mustEqual 3
      }

    "count multiple files in subdirectories using wildcards" in {
      val rootPath = Path.fromString("src/test/resources/uk/gov/tna/dri/fileCountTestFiles/threeFilesinSubDir/")
      val scalaFiles = rootPath.matcher("**/*.jp2")

      rootPath.descendants().collect{ case s@scalaFiles(_) => s }.size mustEqual 3
    }

    "count multiple files in subdirectories, using wildcards" in {
      val rootPath = Path.fromString("src/test/resources/uk/gov/tna/dri/fileCountTestFiles/threeFilesinSubDir/")
      val scalaFiles = rootPath.matcher("**/file*a.jp2")

      rootPath.descendants().collect{ case s@scalaFiles(_) => s }.size mustEqual 3
    }

    "count files without looking in subdirectories, NO wildcards" in {
      val rootPath = Path.fromString("src/test/resources/uk/gov/tna/dri/fileCountTestFiles/threeFilesinSubDir/")
      val scalaFiles = rootPath.matcher("**/file1a.jp2")

      rootPath.descendants().collect{ case s@scalaFiles(_) => s }.size mustEqual 1
    }


  }
}
