/*
 * Copyright (c) 2013, The National Archives digitalpreservation@nationalarchives.gov.uk
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package uk.gov.tna.dri.csv.validator

import org.specs2.mutable.Specification
import uk.gov.tna.dri.csv.validator.schema._
import java.io.{Reader, StringReader}
import scalaz.Success
import scalaz.Failure

class MetaDataValidatorFileCountSpec extends Specification {

  implicit def stringToStringReader(s: String): StringReader = new StringReader(s.replaceAll("\n\\s+", "\n"))

  implicit def stringToSchema(s: String): Schema = {
    val schemaParser = new SchemaParser() {
      val pathSubstitutions = List[(String,String)]()
      override def parse(reader: Reader): ParseResult[Schema] = super.parse(reader) match {
        case s@Success(schema: Schema, _) => s
        case NoSuccess(message, next) => throw new RuntimeException(message)
      }
    }

    schemaParser.parse(s).get
  }

  object TestMetaDataValidator extends AllErrorsMetaDataValidator { val pathSubstitutions = List[(String,String)]() }

  import TestMetaDataValidator._

  "FileCount with path/filename in uk.gov.tna.dri.csv.validator.schema" should {
    "succeed when it find the one file" in {
      val schema =
        """version 1.0
           @totalColumns 2 @noHeader
           File:
           Count: fileCount(file("src/test/resources/uk/gov/tna/dri/uk.gov.tna.dri.csv.validator.schema/checksum.txt"))
        """

      val metaData = """src/test/resources/uk/gov/tna/dri/uk.gov.tna.dri.csv.validator.schema/checksum.txt,1"""

      validate(metaData, schema) must beLike { case Success(_) => ok }
    }

    "succeed when the file count is quoted" in {
      val schema =
        """version 1.0
           @totalColumns 2 @noHeader
           File:
           Count: fileCount(file("src/test/resources/uk/gov/tna/dri/uk.gov.tna.dri.csv.validator.schema/checksum.txt"))
        """

      val metaData = """src/test/resources/uk/gov/tna/dri/uk.gov.tna.dri.csv.validator.schema/checksum.txt,"1""""

      validate(metaData, schema) must beLike { case Success(_) => ok }
    }

    "succeed if column count is optional and missing" in {
      val schema =
        """version 1.0
           @totalColumns 2 @noHeader
           File:
           Count: fileCount(file("src/test/resources/uk/gov/tna/dri/uk.gov.tna.dri.csv.validator.schema/checksum.txt")) @optional
        """

      val metaData = """src/test/resources/uk/gov/tna/dri/uk.gov.tna.dri.csv.validator.schema/checksum.txt,"""

      validate(metaData, schema) must beLike { case Success(_) => ok }
    }

    "succeed if column count is optional and present" in {
      val schema =
        """version 1.0
           @totalColumns 2 @noHeader
           File:
           Count: fileCount(file("src/test/resources/uk/gov/tna/dri/uk.gov.tna.dri.csv.validator.schema/checksum.txt")) @optional
        """

      val metaData = """src/test/resources/uk/gov/tna/dri/uk.gov.tna.dri.csv.validator.schema/checksum.txt,1"""

      validate(metaData, schema) must beLike { case Success(_) => ok }
    }

    "fail when file does not exist" in {
      val schema =
        """version 1.0
           @totalColumns 2 @noHeader
           File:
           Count: fileCount(file("src/test/resources/uk/gov/tna/dri/uk.gov.tna.dri.csv.validator.schema/WRONG.txt"))
        """

      val metaData = """src/test/resources/uk/gov/tna/dri/uk.gov.tna.dri.csv.validator.schema/checksum.txt,1"""

      validate(metaData, schema) must beLike {
        case Failure(messages) => messages.list mustEqual List(ErrorMessage("""fileCount(file("src/test/resources/uk/gov/tna/dri/uk.gov.tna.dri.csv.validator.schema/WRONG.txt")) file "src/test/resources/uk/gov/tna/dri/uk.gov.tna.dri.csv.validator.schema/WRONG.txt" not found for line: 1, column: Count, value: "1""""))
      }
    }

    "fail when wrong number of files found" in {
      val schema =
        """version 1.0
           @totalColumns 2 @noHeader
           File:
           Count: fileCount(file("src/test/resources/uk/gov/tna/dri/uk.gov.tna.dri.csv.validator.schema/checksum.txt"))
        """

      val metaData = """src/test/resources/uk/gov/tna/dri/uk.gov.tna.dri.csv.validator.schema/checksum.txt,2"""

      validate(metaData, schema) must beLike {
        case Failure(messages) => messages.list mustEqual List(ErrorMessage("""fileCount(file("src/test/resources/uk/gov/tna/dri/uk.gov.tna.dri.csv.validator.schema/checksum.txt")) found 1 file(s) for line: 1, column: Count, value: "2""""))
      }
    }


  }

  "FileCount with rootpath and filename in uk.gov.tna.dri.csv.validator.schema" should {
    "succeed when files found does match given cross referenced value" in {
      val schema =
        """version 1.0
           @totalColumns 2 @noHeader
           File:
           Count: fileCount(file("src/test/resources/uk/gov/tna/dri/uk.gov.tna.dri.csv.validator.schema", "checksum.txt"))
        """

      val metaData = """ABC,1"""

      validate(metaData, schema) must beLike { case Success(_) => ok }
    }

    "fail when files found does match given cross referenced value" in {
      val schema =
        """version 1.0
           @totalColumns 2 @noHeader
           File:
           Count: fileCount(file("src/test/resources/uk/gov/tna/dri/uk.gov.tna.dri.csv.validator.schema", "checksum.txt"))
        """

      val metaData = """ABC,99"""

      validate(metaData, schema) must beLike {
        case Failure(messages) => messages.list mustEqual List(ErrorMessage("""fileCount(file("src/test/resources/uk/gov/tna/dri/uk.gov.tna.dri.csv.validator.schema", "checksum.txt")) found 1 file(s) for line: 1, column: Count, value: "99""""))
      }
    }
  }

  "FileCount with root in uk.gov.tna.dri.csv.validator.schema and file in metadata" should {

    "succeed when fileCount does match given root & cross referenced string value" in {
      val schema =
        """version 1.0
           @totalColumns 2 @noHeader
           File:
           Count: fileCount(file("src/test/resources/uk/gov/tna/dri/uk.gov.tna.dri.csv.validator.schema", $File))
        """

      val metaData = """checksum.txt,1"""

      validate(metaData, schema) must beLike { case Success(_) => ok }
    }

    "fail when incorrect root given in uk.gov.tna.dri.csv.validator.schema for root" in {
      val schema =
        """version 1.0
           @totalColumns 2 @noHeader
           File:
           Count: fileCount(file("invalid/path/to/root", $File))
        """

      val metaData = """checksum.txt,99"""

      validate(metaData, schema) must beLike {
        case Failure(messages) => messages.list mustEqual List(ErrorMessage("""fileCount(file("invalid/path/to/root", $File)) incorrect basepath invalid/path/to/root/ (localfile: invalid/path/to/root/checksum.txt) found for line: 1, column: Count, value: "99""""))
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

      val metaData = """src/test/resources/uk/gov/tna/dri/uk.gov.tna.dri.csv.validator.schema/checksum.txt,1"""

      validate(metaData, schema) must beLike { case Success(_) => ok }
    }

    "fail when files found for a crossed reference does not match given value" in {
      val schema =
        """version 1.0
           @totalColumns 2 @noHeader
           File:
           Count: fileCount(file($File))
        """

      val metaData = """src/test/resources/uk/gov/tna/dri/uk.gov.tna.dri.csv.validator.schema/checksum.txt,rubbish"""

      validate(metaData, schema) must beLike {
        case Failure(messages) => messages.list mustEqual List(ErrorMessage("""fileCount(file($File)) 'rubbish' is not a number for line: 1, column: Count, value: "rubbish""""))
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

      val metaData = """src/test/resources/uk/gov/tna/dri/uk.gov.tna.dri.csv.validator.schema,checksum.txt,1"""

      validate(metaData, schema) must beLike { case Success(_) => ok }
    }

    "fail when incorrect root given in metadata" in {
      val schema =
        """version 1.0
           @totalColumns 3 @noHeader
           Root:
           File:
           Count: fileCount(file($Root,$File))
        """

      val metaData = """invalid/path/to/root,checksum.txt,99"""

      validate(metaData, schema) must beLike {
        case Failure(messages) => messages.list mustEqual List(ErrorMessage("""fileCount(file($Root, $File)) incorrect basepath invalid/path/to/root/ (localfile: invalid/path/to/root/checksum.txt) found for line: 1, column: Count, value: "99""""))
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

      val metaData = """src/test/resources/uk/gov/tna/dri/uk.gov.tna.dri.csv.validator.schema,**/checksum.txt,1"""

      validate(metaData, schema) must beLike { case Success(_) => ok }
    }

    "succeed when finding multiple files in sub directories using a '**' wildcard" in {
      val schema =
        """version 1.0
           @totalColumns 3 @noHeader
           Root:
           File:
           Count: fileCount(file($Root, $File))
        """

      val metaData = """src/test/resources/uk/gov/tna/dri/fileCountTestFiles/threeFilesinSubDir,**/*.jp2,3"""

      validate(metaData, schema) must beLike { case Success(_) => ok }
    }


    "succeed when only 1 file is found using a '*' wildcard" in {
      val schema =
        """version 1.0
           @totalColumns 3 @noHeader
           Root:
           File:
           Count: fileCount(file($Root, $File))
        """

      val metaData = """src/test/resources/uk/gov/tna/dri/uk.gov.tna.dri.csv.validator.schema,checksum.*,1"""

      validate(metaData, schema) must beLike { case Success(_) => ok }
    }

    "fail if the optional root contains wildcards" in {
      val schema =
        """version 1.0
           @totalColumns 3 @noHeader
           Root:
           File:
           Count: fileCount(file($Root, $File))
        """

      val metaData = """src/test/resources/uk/gov/tna/dri/**,checksum.*,1"""

      validate(metaData, schema) must beLike {
        case Failure(messages) => messages.list mustEqual List(ErrorMessage("""fileCount(file($Root, $File)) root src/test/resources/uk/gov/tna/dri/**/ (localfile: src/test/resources/uk/gov/tna/dri/**/checksum.*) should not contain wildcards for line: 1, column: Count, value: "1""""))
      }
    }


    "succeed with more than 1 files" in {
      val schema =
        """version 1.0
           @totalColumns 2 @noHeader
           File:
           Count: fileCount(file("src/test/resources/uk/gov/tna/dri/fileCountTestFiles/threeFiles/", $File))
        """

      val metaData = """**/*.jp2,"3""""

      validate(metaData, schema) must beLike { case Success(_) => ok }
    }

    "fail when no files found" in {
      val schema =
        """version 1.0
           @totalColumns 2 @noHeader
           File:
           Count: fileCount(file("src/test/resources/this/is/incorrect", $File))
        """

      val metaData = """**/*.jp2,2"""

      validate(metaData, schema) must beLike {
        case Failure(messages) => messages.list mustEqual List(ErrorMessage("""fileCount(file("src/test/resources/this/is/incorrect", $File)) incorrect basepath src/test/resources/this/is/incorrect/ (localfile: src/test/resources/this/is/incorrect/**/*.jp2) found for line: 1, column: Count, value: "2""""))
      }
    }
  }
}