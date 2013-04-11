package uk.gov.tna.dri.schema

import org.specs2.mutable.Specification
import scalax.file.{Path, PathSet}
import scalaz.Scalaz

class FileWildcardSearchSpec extends Specification {

  class FindBaseTestableFileWildcardSearch extends FileWildcardSearch[Int]{
    import scalaz.Scalaz._

    val pathSubstitutions = List.empty
    def matchSimplePath(fullPath : String) : Scalaz.ValidationNEL[String, Int] = 0.successNel[String]
    def matchWildcardPaths(matchList: PathSet[Path], fullPath: String): Scalaz.ValidationNEL[String, Int] = 0.successNel[String]

    def findBaseWrapper(path: String) : (String, String) = {
      val result = super.findBase(path)
      (result._1.toString, result._2)
    }
  }

  "File Wildcard Search" should {

    "find the correct base path for a Windows file URI starting file:///" in {

      val fileWildcardSearch = new FindBaseTestableFileWildcardSearch

      val base = "file:///C:/x/y/z"
      val file = "file1.jp2"

      fileWildcardSearch.findBaseWrapper(base + "/" + file) mustEqual (base, file)
    }

    "find the correct base path for a Windows file URI starting file:/// with path in file" in {

      val fileWildcardSearch = new FindBaseTestableFileWildcardSearch

      val base = "file:///C:/x/y/z"
      val fileBase = "A/B"
      val file = "file1.jp2"

      fileWildcardSearch.findBaseWrapper(base + "/" + fileBase + "/" + file) mustEqual (base + "/" + fileBase, file)
    }

    "find the correct base path for a Windows file URI starting file:/" in {

      val fileWildcardSearch = new FindBaseTestableFileWildcardSearch

      val base = "file:/C:/x/y/z"
      val file = "file1.jp2"

      fileWildcardSearch.findBaseWrapper(base + "/" + file) mustEqual (base, file)
    }

    "find the correct base path for a Windows file URI starting file:/ with path in file" in {

      val fileWildcardSearch = new FindBaseTestableFileWildcardSearch

      val base = "file:/C:/x/y/z"
      val fileBase = "A/B"
      val file = "file1.jp2"

      fileWildcardSearch.findBaseWrapper(base + "/" + fileBase + "/" + file) mustEqual (base + "/" + fileBase, file)
    }

    "find the correct base path for a Windows file" in {

      val fileWildcardSearch = new FindBaseTestableFileWildcardSearch

      val base = """C:\x\y\z"""
      val file = "file1.jp2"

      fileWildcardSearch.findBaseWrapper(base + """\""" + file) mustEqual (base, file)
    }

    "find the correct base path for a Windows file with path in file" in {

      val fileWildcardSearch = new FindBaseTestableFileWildcardSearch

      val base = """C:\x\y\z"""
      val fileBase = """A\B"""
      val file = "file1.jp2"

      fileWildcardSearch.findBaseWrapper(base + """\""" + fileBase + """\""" + file) mustEqual (base + """\""" + fileBase, file)
    }

    "find the correct base path for a Linux file URI" in {

      val fileWildcardSearch = new FindBaseTestableFileWildcardSearch

      val base = "file:///x/y/z"
      val file = "file1.jp2"

      fileWildcardSearch.findBaseWrapper(base + "/" + file) mustEqual (base, file)
    }

    "find the correct base path for a Linux file URI with path in file" in {

      val fileWildcardSearch = new FindBaseTestableFileWildcardSearch

      val base = "file:///x/y/z"
      val fileBase = "A/B"
      val file = "file1.jp2"

      fileWildcardSearch.findBaseWrapper(base + "/" + fileBase + "/" + file) mustEqual (base + "/" + fileBase, file)
    }

    "find the correct base path for a Linux file" in {

      val fileWildcardSearch = new FindBaseTestableFileWildcardSearch

      val base = "/x/y/z"
      val file = "file1.jp2"

      fileWildcardSearch.findBaseWrapper(base + "/" + file) mustEqual (base, file)
    }

    "find the correct base path for a Linux file with path in file" in {

      val fileWildcardSearch = new FindBaseTestableFileWildcardSearch

      val base = "/x/y/z"
      val fileBase = "A/B"
      val file = "file1.jp2"

      fileWildcardSearch.findBaseWrapper(base + "/" + fileBase + "/" + file) mustEqual (base + "/" + fileBase, file)
    }
  }
}
