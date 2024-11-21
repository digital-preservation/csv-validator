/*
 * Copyright (c) 2013, The National Archives <digitalpreservation@nationalarchives.gov.uk>
 * https://www.nationalarchives.gov.uk
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

import cats.data.Validated
import uk.gov.nationalarchives.csv.validator.schema.Schema
import uk.gov.nationalarchives.csv.validator.Util.TypedPath

@RunWith(classOf[JUnitRunner])
class MetaDataValidatorChecksumSpec extends Specification with TestResources {

  implicit def stringToStringReader(s: String): StringReader = new StringReader(s.replaceAll("\n\\s+", "\n"))

  implicit def stringToSchema(s: String): Schema = {
    val schemaParser = new SchemaParser() {
      val pathSubstitutions = List[(String,String)]()
      val enforceCaseSensitivePathChecks = false
      val trace = false
      val skipFileChecks = false
      override def parse(reader: Reader): ParseResult[Schema] = super.parse(reader) match {
        case s @ Success(schema: Schema, _) =>
          s
        case NoSuccess(message, next) =>
          throw new RuntimeException(s"failed to parse schema [${next.pos.line}:${next.pos.column}}]\n$message\n${next.pos.longString}")
      }
    }

    schemaParser.parse(s).getOrElse(throw new IllegalArgumentException())
  }

  object TestMetaDataValidator extends AllErrorsMetaDataValidator { val pathSubstitutions = List[(String,String)](); val trace = false }

  import TestMetaDataValidator._


  "Checksum with path/filename in schema" should {
    "succeed when calculated algorithm does match given value" in {
      val schema =
        """version 1.0
        @totalColumns 2 @noHeader
        File:
        MD5: checksum(file("""" + checksumPath + """"), "MD5")
                                                 """

      val metaData = s"$checksumPath,232762380299115da6995e4c4ac22fa2"

      validate(metaData, schema, None).isValid mustEqual true
    }

    "fail when calculated algorithm does match not given string value" in {
      val schema =
        """version 1.0
           @totalColumns 2 @noHeader
           File:
           MD5: checksum(file("""" + checksumPath + """"), "MD5")
                                                    """

      val metaData = s"$checksumPath,wrong"

      validate(metaData, schema, None) must beLike {
        case Validated.Invalid(messages) => messages.toList mustEqual List(FailMessage(ValidationError, """checksum(file("""" + checksumPath + """"), "MD5") file """" + checksumPath + """" checksum match fails for line: 1, column: MD5, value: "wrong". Computed checksum value:"232762380299115da6995e4c4ac22fa2"""",Some(1),Some(1)))
      }
    }
  }

  "Checksum with rootpath and filename in schema" should {
    "succeed when calculated algorithm does match given cross referenced string value" in {
      val schema =
        """version 1.0
           @totalColumns 2 @noHeader
           File:
           MD5: checksum(file("""" + schemaPath1_0 + """", "checksum.csvs"), "MD5")
                                                     """

      val metaData = """ABC,232762380299115da6995e4c4ac22fa2"""

      validate(metaData, schema, None).isValid mustEqual true
    }

    "fail when calculated algorithm does match given cross referenced string value" in {
      val schema =
        """version 1.0
           @totalColumns 2 @noHeader
           File:
           MD5: checksum(file("""" + schemaPath1_0 + """", "checksum.csvs"), "MD5")
                                                     """

      val metaData = """ABC,wrong"""

      validate(metaData, schema, None) must beLike {
        case Validated.Invalid(messages) => messages.toList mustEqual List(FailMessage(ValidationError, """checksum(file("""" + schemaPath1_0 + """", "checksum.csvs"), "MD5") file """" + checksumPath + """" checksum match fails for line: 1, column: MD5, value: "wrong". Computed checksum value:"232762380299115da6995e4c4ac22fa2"""",Some(1),Some(1)))
      }
    }
  }

  "Checksum with root in schema and file in metadata" should {

    "succeed when calculated algorithm does match given root & cross referenced string value" in {
      val schema =
        """version 1.0
           @totalColumns 2 @noHeader
           File:
           MD5: checksum(file("""" + schemaPath1_0 + """", $File), "MD5")
                                                     """

      val metaData = """checksum.csvs,232762380299115da6995e4c4ac22fa2"""

      validate(metaData, schema, None).isValid mustEqual true
    }

    "fail when incorrect root given in schema for root" in {
      val schema =
        """version 1.0
           @totalColumns 2 @noHeader
           File:
           MD5: checksum(file("invalid/path/to/root", $File), "MD5")
        """

      val metaData = """checksum.csvs,232762380299115da6995e4c4ac22fa2"""

      validate(metaData, schema, None) must beLike {
        case Validated.Invalid(messages) => messages.toList mustEqual List(FailMessage(ValidationError, """checksum(file("invalid/path/to/root", $File), "MD5") incorrect basepath invalid/path/to/root/ (localfile: """ + TypedPath("invalid/path/to/root/checksum.csvs").toPlatform + """) found for line: 1, column: MD5, value: "232762380299115da6995e4c4ac22fa2"""",Some(1),Some(1)))
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

        val metaData = s"$checksumPath,232762380299115da6995e4c4ac22fa2"

        validate(metaData, schema, None).isValid mustEqual true
      }

      "fail when calculated algorithm does not match given cross referenced string value" in {
        val schema =
          """version 1.0
           @totalColumns 2 @noHeader
           File:
           MD5: checksum(file($File), "MD5")
          """

        val metaData = s"$checksumPath,rubbish"

        validate(metaData, schema, None) must beLike {
          case Validated.Invalid(messages) => messages.toList mustEqual List(FailMessage(ValidationError, """checksum(file($File), "MD5") file """" + checksumPath + """" checksum match fails for line: 1, column: MD5, value: "rubbish". Computed checksum value:"232762380299115da6995e4c4ac22fa2"""",Some(1),Some(1)))
        }
      }
    }

    "fail when calculated algorithm does match" in {
      val schema =
        """version 1.0
           @totalColumns 2 @noHeader
           File:
           MD5: checksum(file("""" + schemaPath1_0 + """", $File), "MD5")
                                                     """

      val metaData = """checksum.csvs,rubbish"""

      validate(metaData, schema, None) must beLike {
        case Validated.Invalid(messages) => messages.toList mustEqual List(FailMessage(ValidationError, """checksum(file("""" + schemaPath1_0 + """", $File), "MD5") file """" + checksumPath + """" checksum match fails for line: 1, column: MD5, value: "rubbish". Computed checksum value:"232762380299115da6995e4c4ac22fa2"""",Some(1),Some(1)))
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

      val metaData = s"$schemaPath1_0,checksum.csvs,232762380299115da6995e4c4ac22fa2"

      validate(metaData, schema, None).isValid mustEqual true
    }

    "fail when incorrect root given in metadata" in {
      val schema =
        """version 1.0
           @totalColumns 3 @noHeader
           Root:
           File:
           MD5: checksum(file($Root,$File), "MD5")
        """

      val metaData = """invalid/path/to/root,checksum.csvs,232762380299115da6995e4c4ac22fa2"""

      validate(metaData, schema, None) must beLike {
        case Validated.Invalid(messages) => messages.toList mustEqual List(FailMessage(ValidationError, """checksum(file($Root, $File), "MD5") incorrect basepath invalid/path/to/root/ (localfile: """ + TypedPath("invalid/path/to/root/checksum.csvs").toPlatform + """) found for line: 1, column: MD5, value: "232762380299115da6995e4c4ac22fa2"""", Some(1), Some(2)))
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

      val metaData = schemaPath1_0 + "," + "**" + FILE_SEPARATOR + "checksum.csvs,232762380299115da6995e4c4ac22fa2"

      validate(metaData, schema, None).isValid mustEqual true
    }

    "succeed when only 1 file is found using a '*' wildcard" in {
      val schema =
        """version 1.0
           @totalColumns 3 @noHeader
           Root:
           File:
           MD5: checksum(file($Root, $File), "MD5")
        """

      val metaData = s"$schemaPath1_0,checksum.*,232762380299115da6995e4c4ac22fa2"

      validate(metaData, schema, None).isValid mustEqual true
    }

    "fail if the optional root contains wildcards" in {
      val schema =
        """version 1.0
           @totalColumns 3 @noHeader
           Root:
           File:
           MD5: checksum(file($Root, $File), "MD5")
        """

      val searchPath = baseResourcePkgPath + FILE_SEPARATOR + "**"

      val metaData = s"$searchPath,checksum.*,232762380299115da6995e4c4ac22fa2"

      validate(metaData, schema, None) must beLike {
        case Validated.Invalid(messages) =>
          val msg = """checksum(file($Root, $File), "MD5") root """ + searchPath + FILE_SEPARATOR + """ (localfile: """ + searchPath + FILE_SEPARATOR + """checksum.*) should not contain wildcards for line: 1, column: MD5, value: "232762380299115da6995e4c4ac22fa2""""
          messages.toList mustEqual List(FailMessage(ValidationError, msg, Some(1), Some(2)))
      }
    }


    "fail with more than 1 files" in {
      val schema =
        """version 1.0
           @totalColumns 2 @noHeader
           File:
           MD5: checksum(file("""" + threeFilesPath + """", $File), "MD5")
                                                      """

      val metaData = """**/*.jp2,"232762380299115da6995e4c4ac22fa2""""

      validate(metaData, schema, None) must beLike {
        case Validated.Invalid(messages) => messages.toList mustEqual List(FailMessage(ValidationError, """checksum(file("""" + threeFilesPath + """", $File), "MD5") multiple files for """ + threeFilesPath + s"""${FILE_SEPARATOR}**${FILE_SEPARATOR}*.jp2 found for line: 1, column: MD5, value: "232762380299115da6995e4c4ac22fa2"""",Some(1),Some(1)))
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

      validate(metaData, schema, None) must beLike {
        case Validated.Invalid(messages) => messages.toList mustEqual List(FailMessage(ValidationError, """checksum(file("src/test/resources/this/is/incorrect", $File), "MD5") incorrect basepath src/test/resources/this/is/incorrect/ (localfile: """ + TypedPath("src/test/resources/this/is/incorrect/**/*.jp2").toPlatform + """) found for line: 1, column: MD5, value: "232762380299115da6995e4c4ac22fa2"""",Some(1),Some(1)))
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

      val metaData = s"$checksumPath,232762380299115da6995e4c4ac22fa2"

      validate(metaData, schema, None).isValid mustEqual true
    }
  }
}