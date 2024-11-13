/*
 * Copyright (c) 2013, The National Archives <digitalpreservation@nationalarchives.gov.uk>
 * https://www.nationalarchives.gov.uk
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package uk.gov.nationalarchives.csv.validator.schema.v1_0

import org.specs2.mutable.Specification
import cats.data.{Validated, ValidatedNel}
import cats.syntax.validated._

import java.nio.file.Path

class FileWildcardSearchSpec extends Specification {

  class FindBaseTestableFileWildcardSearch extends FileWildcardSearch[Int]{
    import cats.data.Validated

    val pathSubstitutions = List.empty
    def matchSimplePath(fullPath : String) : ValidatedNel[String, Int] = 0.validNel[String]
    def matchWildcardPaths(matchList: Seq[Path], fullPath: String): ValidatedNel[String, Int] = 0.validNel[String]

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
