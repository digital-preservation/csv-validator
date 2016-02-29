/**
 * Copyright (c) 2013, The National Archives <digitalpreservation@nationalarchives.gov.uk>
 * http://www.nationalarchives.gov.uk
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package uk.gov.nationalarchives.csv.validator

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import uk.gov.nationalarchives.csv.validator.schema._
import java.io.{Reader, StringReader}
import scalaz.Success
import scalaz.Failure
import uk.gov.nationalarchives.csv.validator.Util.TypedPath

@RunWith(classOf[JUnitRunner])
class MetaDataValidatorFileCountSpec extends Specification with TestResources {

  implicit def stringToStringReader(s: String): StringReader = new StringReader(s.replaceAll("\n\\s+", "\n"))

  implicit def stringToSchema(s: String): Schema = {
    val schemaParser = new SchemaParser() {
      val pathSubstitutions = List[(String,String)]()
      val enforceCaseSensitivePathChecks = false
      val trace = false
      override def parse(reader: Reader): ParseResult[Schema] = super.parse(reader) match {
        case s@Success(schema: Schema, _) => s
        case NoSuccess(message, next) => throw new RuntimeException(message)
      }
    }

    schemaParser.parse(s).get
  }

  object TestMetaDataValidator extends AllErrorsMetaDataValidator { val pathSubstitutions = List[(String,String)](); val trace = false }

  import TestMetaDataValidator._

  "FileCount with path/filename in schema" should {
    "succeed when it finds the one file" in {
      val schema =
        """version 1.0
           @totalColumns 2 @noHeader
           File:
           Count: fileCount(file("""" + checksumPath + """"))
        """

      val metaData = s"$checksumPath,1"

      validate(metaData, schema, None) must beLike { case Success(_) => ok }
    }

    "succeed when the file count is quoted" in {
      val schema =
        """version 1.0
           @totalColumns 2 @noHeader
           File:
           Count: fileCount(file("""" + checksumPath + """"))
        """

      val metaData = checksumPath + ""","1""""

      validate(metaData, schema, None) must beLike { case Success(_) => ok }
    }

    "succeed if column count is optional and missing" in {
      val schema =
        """version 1.0
           @totalColumns 2 @noHeader
           File:
           Count: fileCount(file("""" + checksumPath + """")) @optional
        """

      val metaData = s"$checksumPath,"

      validate(metaData, schema, None) must beLike { case Success(_) => ok }
    }

    "succeed if column count is optional and present" in {
      val schema =
        """version 1.0
           @totalColumns 2 @noHeader
           File:
           Count: fileCount(file("""" + checksumPath +  """")) @optional
        """

      val metaData = s"$checksumPath,1"

      validate(metaData, schema, None) must beLike { case Success(_) => ok }
    }

    "fail when file does not exist" in {

      val wrongPath = resourcePath("schema/WRONG.csvs")

      val schema =
        """version 1.0
           @totalColumns 2 @noHeader
           File:
           Count: fileCount(file("""" + wrongPath + """"))
        """

      val metaData = s"$checksumPath,1"

      validate(metaData, schema, None) must beLike {
        case Failure(messages) => messages.list mustEqual List(ErrorMessage("""fileCount(file("""" + wrongPath + """")) file """" + wrongPath + """" not found for line: 1, column: Count, value: "1"""",Some(1),Some(1)))
      }
    }

    "fail when wrong number of files found" in {
      val schema =
        """version 1.0
           @totalColumns 2 @noHeader
           File:
           Count: fileCount(file("""" + checksumPath + """"))
        """

      val metaData = s"$checksumPath,2"

      validate(metaData, schema, None) must beLike {
        case Failure(messages) => messages.list mustEqual List(ErrorMessage("""fileCount(file("""" + checksumPath + """")) found 1 file(s) for line: 1, column: Count, value: "2"""",Some(1),Some(1)))
      }
    }
  }

  "FileCount with rootpath and filename in schema" should {

    "succeed when files found does match given cross referenced value" in {
      val schema =
        """version 1.0
           @totalColumns 2 @noHeader
           File:
           Count: fileCount(file("""" + schemaPath1_0 + """", "checksum.csvs"))
        """

      val metaData = """ABC,1"""

      validate(metaData, schema, None) must beLike { case Success(_) => ok }
    }

    "fail when files found does match given cross referenced value" in {
      val schema =
        """version 1.0
           @totalColumns 2 @noHeader
           File:
           Count: fileCount(file("""" + schemaPath1_0 + """", "checksum.csvs"))
        """

      val metaData = """ABC,99"""

      validate(metaData, schema, None) must beLike {
        case Failure(messages) => messages.list mustEqual List(ErrorMessage("""fileCount(file("""" + schemaPath1_0 + """", "checksum.csvs")) found 1 file(s) for line: 1, column: Count, value: "99"""",Some(1),Some(1)))
      }
    }
  }

  "FileCount with root in schema and file in metadata" should {

    "succeed when fileCount does match given root & cross referenced string value" in {
      val schema =
        """version 1.0
           @totalColumns 2 @noHeader
           File:
           Count: fileCount(file("""" + schemaPath1_0 + """", $File))
        """

      val metaData = """checksum.csvs,1"""

      validate(metaData, schema, None) must beLike { case Success(_) => ok }
    }

    "fail when incorrect root given in schema for root" in {
      val schema =
        """version 1.0
           @totalColumns 2 @noHeader
           File:
           Count: fileCount(file("invalid/path/to/root", $File))
        """

      val metaData = """checksum.csvs,99"""

      validate(metaData, schema, None) must beLike {
        case Failure(messages) => messages.list mustEqual List(ErrorMessage("""fileCount(file("invalid/path/to/root", $File)) incorrect basepath invalid/path/to/root/ (localfile: """ + TypedPath("invalid/path/to/root/checksum.csvs").toPlatform + """) found for line: 1, column: Count, value: "99"""",Some(1),Some(1)))
      }
    }
  }


  "FileCount with fullpath in metedata" should {

    "succeed when files found for a crossed reference does match given value" in {
      val schema =
        """version 1.0
           @totalColumns 2 @noHeader
           File:
           Count: fileCount(file($File))
        """

      val metaData = s"$checksumPath,1"

      validate(metaData, schema, None) must beLike { case Success(_) => ok }
    }

    "fail when files found for a crossed reference does not match given value" in {
      val schema =
        """version 1.0
           @totalColumns 2 @noHeader
           File:
           Count: fileCount(file($File))
        """

      val metaData = s"$checksumPath,rubbish"

      validate(metaData, schema, None) must beLike {
        case Failure(messages) => messages.list mustEqual List(ErrorMessage("""fileCount(file($File)) 'rubbish' is not a number for line: 1, column: Count, value: "rubbish"""",Some(1),Some(1)))
      }
    }
  }


  "FileCount with cross reference on both root and file" should {

    "succeed when root and file referance a valid file" in {
      val schema =
        """version 1.0
           @totalColumns 3 @noHeader
           Root:
           File:
           Count: fileCount(file($Root, $File))
        """

      val metaData = s"$schemaPath1_0,checksum.csvs,1"

      validate(metaData, schema, None) must beLike { case Success(_) => ok }
    }

    "fail when incorrect root given in metadata" in {
      val schema =
        """version 1.0
           @totalColumns 3 @noHeader
           Root:
           File:
           Count: fileCount(file($Root,$File))
        """

      val metaData = """invalid/path/to/root,checksum.csvs,99"""

      validate(metaData, schema, None) must beLike {
        case Failure(messages) => messages.list mustEqual List(ErrorMessage("""fileCount(file($Root, $File)) incorrect basepath invalid/path/to/root/ (localfile: """ + TypedPath("invalid/path/to/root/checksum.csvs").toPlatform + """) found for line: 1, column: Count, value: "99"""",Some(1),Some(2)))
      }
    }
  }

  "FileCount with multi files matches" should {

    "succeed when only 1 file is found using a '**' wildcard" in {
      val schema =
        """version 1.0
           @totalColumns 3 @noHeader
           Root:
           File:
           Count: fileCount(file($Root, $File))
        """

      val metaData = s"$schemaPath1_0,**/checksum.csvs,1"

      validate(metaData, schema, None) must beLike { case Success(_) => ok }
    }

    "succeed when finding multiple files in sub directories using a '**' wildcard" in {

      val schema =
        """version 1.0
           @totalColumns 3 @noHeader
           Root:
           File:
           Count: fileCount(file($Root, $File))
        """

      val metaData = s"$threeFilesInSubDirPath,**/*.jp2,3"

      validate(metaData, schema, None) must beLike { case Success(_) => ok }
    }

    "succeed when only 1 file is found using a '*' wildcard" in {
      val schema =
        """version 1.0
           @totalColumns 3 @noHeader
           Root:
           File:
           Count: fileCount(file($Root, $File))
        """

      val metaData = s"$schemaPath1_0,checksum.*,1"

      validate(metaData, schema, None) must beLike { case Success(_) => ok }
    }

    "fail if the optional root contains wildcards" in {
      val schema =
        """version 1.0
           @totalColumns 3 @noHeader
           Root:
           File:
           Count: fileCount(file($Root, $File))
        """

      val searchPath = baseResourcePkgPath + FILE_SEPARATOR + "**"

      val metaData = s"$searchPath,checksum.*,1"

      validate(metaData, schema, None) must beLike {
        case Failure(messages) =>
          messages.list mustEqual List(ErrorMessage("""fileCount(file($Root, $File)) root """ + searchPath + """/ (localfile: """ + searchPath + FILE_SEPARATOR + """checksum.*) should not contain wildcards for line: 1, column: Count, value: "1"""",Some(1),Some(2)))
      }
    }

    "succeed with more than 1 files" in {
      val schema =
        """version 1.0
           @totalColumns 2 @noHeader
           File:
           Count: fileCount(file("""" + threeFilesPath + """", $File))
        """

      val metaData = """**/*.jp2,"3""""

      validate(metaData, schema, None) must beLike { case Success(_) => ok }
    }

    "fail when no files found" in {
      val schema =
        """version 1.0
           @totalColumns 2 @noHeader
           File:
           Count: fileCount(file("src/test/resources/this/is/incorrect", $File))
        """

      val metaData = """**/*.jp2,2"""

      validate(metaData, schema, None) must beLike {
        case Failure(messages) => messages.list mustEqual List(ErrorMessage("""fileCount(file("src/test/resources/this/is/incorrect", $File)) incorrect basepath src/test/resources/this/is/incorrect/ (localfile: """ + TypedPath("src/test/resources/this/is/incorrect/**/*.jp2").toPlatform + """) found for line: 1, column: Count, value: "2"""",Some(1),Some(1)))
      }
    }
  }
}