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
import uk.gov.tna.dri.csv.validator.schema.Schema
import scalaz.Failure

class MetaDataValidatorChecksumSpec extends Specification {

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

  "Checksum with path/filename in uk.gov.tna.dri.csv.validator.schema" should {
    "succeed when calculated algorithm does match given value" in {
      val schema =
        """version 1.0
           @totalColumns 2 @noHeader
           File:
           MD5: checksum(file("src/test/resources/uk/gov/tna/dri/uk.gov.tna.dri.csv.validator.schema/checksum.txt"), "MD5")
        """

      val metaData = """src/test/resources/uk/gov/tna/dri/uk.gov.tna.dri.csv.validator.schema/checksum.txt,232762380299115da6995e4c4ac22fa2"""

      validate(metaData, schema) must beLike { case Success(_) => ok }
    }

    "fail when calculated algorithm does match not given string value" in {
      val schema =
        """version 1.0
           @totalColumns 2 @noHeader
           File:
           MD5: checksum(file("src/test/resources/uk/gov/tna/dri/uk.gov.tna.dri.csv.validator.schema/checksum.txt"), "MD5")
        """

      val metaData = """src/test/resources/uk/gov/tna/dri/uk.gov.tna.dri.csv.validator.schema/checksum.txt,wrong"""

      validate(metaData, schema) must beLike {
        case Failure(messages) => messages.list mustEqual List(ErrorMessage("""checksum(file("src/test/resources/uk/gov/tna/dri/uk.gov.tna.dri.csv.validator.schema/checksum.txt"), "MD5") file "src/test/resources/uk/gov/tna/dri/uk.gov.tna.dri.csv.validator.schema/checksum.txt" checksum match fails for line: 1, column: MD5, value: "wrong". Computed checksum value:"232762380299115da6995e4c4ac22fa2""""))
      }
    }
  }

  "Checksum with rootpath and filename in uk.gov.tna.dri.csv.validator.schema" should {
    "succeed when calculated algorithm does match given cross referenced string value" in {
      val schema =
        """version 1.0
           @totalColumns 2 @noHeader
           File:
           MD5: checksum(file("src/test/resources/uk/gov/tna/dri/uk.gov.tna.dri.csv.validator.schema", "checksum.txt"), "MD5")
        """

      val metaData = """ABC,232762380299115da6995e4c4ac22fa2"""

      validate(metaData, schema) must beLike { case Success(_) => ok }
    }

    "fail when calculated algorithm does match given cross referenced string value" in {
      val schema =
        """version 1.0
           @totalColumns 2 @noHeader
           File:
           MD5: checksum(file("src/test/resources/uk/gov/tna/dri/uk.gov.tna.dri.csv.validator.schema", "checksum.txt"), "MD5")
        """

      val metaData = """ABC,wrong"""

      validate(metaData, schema) must beLike {
        case Failure(messages) => messages.list mustEqual List(ErrorMessage("""checksum(file("src/test/resources/uk/gov/tna/dri/uk.gov.tna.dri.csv.validator.schema", "checksum.txt"), "MD5") file "src/test/resources/uk/gov/tna/dri/uk.gov.tna.dri.csv.validator.schema/checksum.txt" checksum match fails for line: 1, column: MD5, value: "wrong". Computed checksum value:"232762380299115da6995e4c4ac22fa2""""))
      }
    }
  }

  "Checksum with root in uk.gov.tna.dri.csv.validator.schema and file in metadata" should {

    "succeed when calculated algorithm does match given root & cross referenced string value" in {
      val schema =
        """version 1.0
           @totalColumns 2 @noHeader
           File:
           MD5: checksum(file("src/test/resources/uk/gov/tna/dri/uk.gov.tna.dri.csv.validator.schema", $File), "MD5")
        """

      val metaData = """checksum.txt,232762380299115da6995e4c4ac22fa2"""

      validate(metaData, schema) must beLike { case Success(_) => ok }
    }

    "fail when incorrect root given in uk.gov.tna.dri.csv.validator.schema for root" in {
      val schema =
        """version 1.0
           @totalColumns 2 @noHeader
           File:
           MD5: checksum(file("invalid/path/to/root", $File), "MD5")
        """

      val metaData = """checksum.txt,232762380299115da6995e4c4ac22fa2"""

      validate(metaData, schema) must beLike {
        case Failure(messages) => messages.list mustEqual List(ErrorMessage("""checksum(file("invalid/path/to/root", $File), "MD5") incorrect basepath invalid/path/to/root/ (localfile: invalid/path/to/root/checksum.txt) found for line: 1, column: MD5, value: "232762380299115da6995e4c4ac22fa2""""))
      }
    }

  "Checksum with fullpath in metedata" should {

    "succeed when calculated algorithm does match given cross referenced string value" in {
      val schema =
        """version 1.0
           @totalColumns 2 @noHeader
           File:
           MD5: checksum(file($File), "MD5")
        """

      val metaData = """src/test/resources/uk/gov/tna/dri/uk.gov.tna.dri.csv.validator.schema/checksum.txt,232762380299115da6995e4c4ac22fa2"""

      validate(metaData, schema) must beLike { case Success(_) => ok }
    }

    "fail when calculated algorithm does not match given cross referenced string value" in {
      val schema =
        """version 1.0
           @totalColumns 2 @noHeader
           File:
           MD5: checksum(file($File), "MD5")
        """

      val metaData = """src/test/resources/uk/gov/tna/dri/uk.gov.tna.dri.csv.validator.schema/checksum.txt,rubbish"""

      validate(metaData, schema) must beLike {
        case Failure(messages) => messages.list mustEqual List(ErrorMessage("""checksum(file($File), "MD5") file "src/test/resources/uk/gov/tna/dri/uk.gov.tna.dri.csv.validator.schema/checksum.txt" checksum match fails for line: 1, column: MD5, value: "rubbish". Computed checksum value:"232762380299115da6995e4c4ac22fa2""""))
      }
    }
  }

    "fail when calculated algorithm does match" in {
      val schema =
        """version 1.0
           @totalColumns 2 @noHeader
           File:
           MD5: checksum(file("src/test/resources/uk/gov/tna/dri/uk.gov.tna.dri.csv.validator.schema", $File), "MD5")
        """

      val metaData = """checksum.txt,rubbish"""

      validate(metaData, schema) must beLike {
        case Failure(messages) => messages.list mustEqual List(ErrorMessage("""checksum(file("src/test/resources/uk/gov/tna/dri/uk.gov.tna.dri.csv.validator.schema", $File), "MD5") file "src/test/resources/uk/gov/tna/dri/uk.gov.tna.dri.csv.validator.schema/checksum.txt" checksum match fails for line: 1, column: MD5, value: "rubbish". Computed checksum value:"232762380299115da6995e4c4ac22fa2""""))
      }
    }
  }

  "Checksum with cross reference on both root and file" should {

    "succeed when root and file referance a valid file" in {
      val schema =
        """version 1.0
           @totalColumns 3 @noHeader
           Root:
           File:
           MD5: checksum(file($Root, $File), "MD5")
        """

      val metaData = """src/test/resources/uk/gov/tna/dri/uk.gov.tna.dri.csv.validator.schema,checksum.txt,232762380299115da6995e4c4ac22fa2"""

      validate(metaData, schema) must beLike { case Success(_) => ok }
    }

    "fail when incorrect root given in metadata" in {
      val schema =
        """version 1.0
           @totalColumns 3 @noHeader
           Root:
           File:
           MD5: checksum(file($Root,$File), "MD5")
        """

      val metaData = """invalid/path/to/root,checksum.txt,232762380299115da6995e4c4ac22fa2"""

      validate(metaData, schema) must beLike {
        case Failure(messages) => messages.list mustEqual List(ErrorMessage("""checksum(file($Root, $File), "MD5") incorrect basepath invalid/path/to/root/ (localfile: invalid/path/to/root/checksum.txt) found for line: 1, column: MD5, value: "232762380299115da6995e4c4ac22fa2""""))
      }
    }
  }

  "Checksum with multi files matches" should {

    "succeed when only 1 file is found using a '**' wildcard" in {
      val schema =
        """version 1.0
           @totalColumns 3 @noHeader
           Root:
           File:
           MD5: checksum(file($Root, $File), "MD5")
        """

      val metaData = """src/test/resources/uk/gov/tna/dri/uk.gov.tna.dri.csv.validator.schema,**/checksum.txt,232762380299115da6995e4c4ac22fa2"""

      validate(metaData, schema) must beLike { case Success(_) => ok }
    }

    "succeed when only 1 file is found using a '*' wildcard" in {
      val schema =
        """version 1.0
           @totalColumns 3 @noHeader
           Root:
           File:
           MD5: checksum(file($Root, $File), "MD5")
        """

      val metaData = """src/test/resources/uk/gov/tna/dri/uk.gov.tna.dri.csv.validator.schema,checksum.*,232762380299115da6995e4c4ac22fa2"""

      validate(metaData, schema) must beLike { case Success(_) => ok }
    }

    "fail if the optional root contains wildcards" in {
      val schema =
        """version 1.0
           @totalColumns 3 @noHeader
           Root:
           File:
           MD5: checksum(file($Root, $File), "MD5")
        """

      val metaData = """src/test/resources/uk/gov/tna/dri/**,checksum.*,232762380299115da6995e4c4ac22fa2"""

      validate(metaData, schema) must beLike {
        case Failure(messages) => messages.list mustEqual List(ErrorMessage("""checksum(file($Root, $File), "MD5") root src/test/resources/uk/gov/tna/dri/**/ (localfile: src/test/resources/uk/gov/tna/dri/**/checksum.*) should not contain wildcards for line: 1, column: MD5, value: "232762380299115da6995e4c4ac22fa2""""))
      }
    }


    "fail with more than 1 files" in {
      val schema =
        """version 1.0
           @totalColumns 2 @noHeader
           File:
           MD5: checksum(file("src/test/resources/uk/gov/tna/dri/fileCountTestFiles/threeFiles/", $File), "MD5")
        """

      val metaData = """**/*.jp2,"232762380299115da6995e4c4ac22fa2""""

      validate(metaData, schema) must beLike {
        case Failure(messages) => messages.list mustEqual List(ErrorMessage("""checksum(file("src/test/resources/uk/gov/tna/dri/fileCountTestFiles/threeFiles/", $File), "MD5") multiple files for src/test/resources/uk/gov/tna/dri/fileCountTestFiles/threeFiles/**/*.jp2 found for line: 1, column: MD5, value: "232762380299115da6995e4c4ac22fa2""""))
      }
    }

    "fail when no files found" in {
      val schema =
        """version 1.0
           @totalColumns 2 @noHeader
           File:
           MD5: checksum(file("src/test/resources/this/is/incorrect", $File), "MD5")
        """

      val metaData = """**/*.jp2,"232762380299115da6995e4c4ac22fa2""""

      validate(metaData, schema) must beLike {
        case Failure(messages) => messages.list mustEqual List(ErrorMessage("""checksum(file("src/test/resources/this/is/incorrect", $File), "MD5") incorrect basepath src/test/resources/this/is/incorrect/ (localfile: src/test/resources/this/is/incorrect/**/*.jp2) found for line: 1, column: MD5, value: "232762380299115da6995e4c4ac22fa2""""))
      }
    }
  }


  "Checksum with an algorithm" should {

    "succeed when using a valid algorithm" in {
      val schema =
        """version 1.0
           @totalColumns 2 @noHeader
           File:
           MD5: checksum(file($File), "MD5")
        """

      val metaData = """src/test/resources/uk/gov/tna/dri/uk.gov.tna.dri.csv.validator.schema/checksum.txt,232762380299115da6995e4c4ac22fa2"""

      validate(metaData, schema) must beLike { case Success(_) => ok }
    }
  }
}