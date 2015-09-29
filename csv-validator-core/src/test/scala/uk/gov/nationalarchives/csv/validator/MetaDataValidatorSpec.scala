/**
 * Copyright (c) 2013, The National Archives <digitalpreservation@nationalarchives.gov.uk>
 * http://www.nationalarchives.gov.uk
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package uk.gov.nationalarchives.csv.validator

import org.specs2.mutable.Specification
import uk.gov.nationalarchives.csv.validator.schema._
import java.io.{Reader, StringReader}
import uk.gov.nationalarchives.csv.validator.schema.Schema
import scalaz.Success
import scalaz.Failure

class MetaDataValidatorSpec extends Specification with TestResources {

  implicit def stringToStringReader(s: String): StringReader = new StringReader(s.replaceAll("\n\\s+", "\n"))

  implicit def stringToSchema(s: String): Schema = {
    val schemaParser = new SchemaParser() {
      val pathSubstitutions = List[(String,String)]()
      val enforceCaseSensitivePathChecks = false
      val trace = false
      override def parse(reader: Reader): ParseResult[Schema] = {
        super.parse(reader) match {
          case s @ Success(schema: Schema, _) => s
          case NoSuccess(message, next) => throw new RuntimeException(message)
        }
      }
    }

    schemaParser.parse(s).get
  }

  object TestMetaDataValidator extends AllErrorsMetaDataValidator { val pathSubstitutions = List[(String,String)](); val trace = false }

  import TestMetaDataValidator._

  val mustExistForRulePath = resourcePath("schema/mustExistForRule.csvs")

  "Validation" should {

    "succeed for correct total columns for multiple lines" in {
      val schema =
        """version 1.0
           @totalColumns 2 @noHeader
           column1:
           column2:
        """

      val metaData =
        """col1, col2
           col1, col2
        """

      validate(metaData, schema, None) must beLike { case Success(_) => ok }
    }

    "fail for incorrect number of total columns for multiple lines" in {
      val schema =
        """version 1.0
           @totalColumns 3 @noHeader
           column1:
           column2:
           column3:
        """

      val metaData =
        """col1, col2, col3
           col1, col2
           col1, col2, col3
        """

      validate(metaData, schema, None) must beLike {
        case Failure(messages) => messages.list mustEqual List(ErrorMessage("Expected @totalColumns of 3 and found 2 on line 2",Some(2),Some(2)), ErrorMessage("Missing value at line: 2, column: column3",Some(2),Some(2)))
      }
    }

    "fail if columns on multiple rows do not pass" in {
      val schema =
        """version 1.0
           @totalColumns 2 @noHeader
           first: regex("[3-8]*")
           second: regex("[a-c]*")
        """

      val metaData =
        """34,xxxy
           abcd,uii
        """

      validate(metaData, schema, None) must beLike {
        case Failure(messages) => messages.list mustEqual List (
          ErrorMessage("""regex("[a-c]*") fails for line: 1, column: second, value: "xxxy"""",Some(1),Some(1)),
          ErrorMessage("""regex("[3-8]*") fails for line: 2, column: first, value: "abcd"""",Some(2),Some(0)),
          ErrorMessage("""regex("[a-c]*") fails for line: 2, column: second, value: "uii"""",Some(2),Some(1)))
      }
    }

    "succeed for multiple rows" in {
      val schema =
        """version 1.0
           @totalColumns 2 @noHeader
           col1:
           col2WithRule: regex("[0-9]*")
        """

      val metaData =
        """someData,345
           someMore,12
        """

      validate(metaData, schema, None) must beLike { case Success(_) => ok }
    }

    "fail for a single rule" in {
      val schema =
        """version 1.0
           @totalColumns 2 @noHeader
           Col1: regex("C11")
           Col2:
        """

      val metaData = "c11,c12"

      validate(metaData, schema, None) should beLike {
        case Failure(messages) => messages.list mustEqual List(ErrorMessage("""regex("C11") fails for line: 1, column: Col1, value: "c11"""",Some(1),Some(0)))
      }
    }

    "fail for rule when cell missing" in {
      val schema =
        """version 1.0
           @totalColumns 2 @noHeader
           Col1:
           Col2: regex("[0-9]")
        """

      val metaData = "1"

      validate(metaData, schema, None) should beLike {
        case Failure(messages) => messages.list mustEqual List(ErrorMessage("Expected @totalColumns of 2 and found 1 on line 1",Some(1),Some(1)), ErrorMessage("Missing value at line: 1, column: Col2",Some(1),Some(1)))
      }
    }

    "succeed for more than one regex" in {
      val schema =
        """version 1.0
           @totalColumns 1 @noHeader
           c1: regex("\w+") regex("^S.+")
        """

      val metaData = """Scooby"""

      validate(metaData, schema, None) must beLike { case Success(_) => ok }
    }

    "fail when at least one regex fails for multiple regex provided in a column definition" in {
      val schema =
        """version 1.0
           @totalColumns 1 @noHeader
           c1: regex("\w+") regex("^T.+") regex("^X.+")
        """

      val metaData = """Scooby"""

      validate(metaData, schema, None) must beLike {
        case Failure(messages) => messages.list mustEqual List(ErrorMessage("""regex("^T.+") fails for line: 1, column: c1, value: "Scooby"""",Some(1),Some(0)), ErrorMessage("""regex("^X.+") fails for line: 1, column: c1, value: "Scooby"""",Some(1),Some(0)))
      }
    }

    "succeed for multiple rows with InRule as string literal" in {
      val schema =
        """version 1.0
           @totalColumns 2 @noHeader
           col1:
           col2WithRule: regex("[0-9a-z]*") in("345dog")
        """

      val metaData =
        """someData,dog
           someMore,dog
        """

      validate(metaData, schema, None) must beLike { case Success(_) => ok }
    }

    "succeed for InRule as column reference" in {
      val schema =
        """version 1.0
           @totalColumns 2 @noHeader
           col1:
           col2WithRule: regex("\w*") in($col1)
        """

      val metaData =
        """blah_mustBeIn_blah,mustBeIn
           blah_andMustBeIn,andMustBeIn"""
      validate(metaData, schema, None) must beLike { case Success(_) => ok }
    }

    "fail for InRule as column reference where case does not match" in {
      val schema =
        """version 1.0
           @totalColumns 2 @noHeader
           col1:
           col2WithRule: regex("\w*") in($col1)
        """

      val metaData =
        """blah_MUSTBEIN_blah,mustBeIn
           blah_andMustBeIn,andMustBeIn"""

      validate(metaData, schema, None) should beLike {
        case Failure(messages) => messages.list mustEqual List(ErrorMessage("""in($col1) fails for line: 1, column: col2WithRule, value: "mustBeIn"""",Some(1),Some(1)))
      }
    }

    "succeed for InRule as column reference where case is ignored" in {
      val schema =
        """version 1.0
           @totalColumns 2 @noHeader
           col1:
           col2WithRule: regex("\w*") in($col1) @ignoreCase
        """

      val metaData =
        """blah_MUSTBEIN_blah,mustBeIn
           blah_andMustBeIn,andMustBeIn"""

      validate(metaData, schema, None) must beLike { case Success(_) => ok }
    }

    "succeed when @optional is given for an empty cell" in {
      val schema =
        """version 1.0
           @totalColumns 3 @noHeader
           Col1:
           Col2: regex("[0-9]") @optional
           Col3:
        """

      val metaData = "1, , 3"

      validate(metaData, schema, None) must beLike { case Success(_) => ok }
    }

    "fail when @optional is given for non empty cell with a failing rule" in {
      val schema =
        """version 1.0
           @totalColumns 3 @noHeader
           Col1:
           Col2: regex("[0-9]") @optional
           Col3:
        """

      val metaData = "1,a,3"

      validate(metaData, schema, None) should beLike {
        case Failure(messages) => messages.list mustEqual List(ErrorMessage("""regex("[0-9]") fails for line: 1, column: Col2, value: "a"""",Some(1),Some(1)))
      }
    }

    "pass for empty cell with @optional but fail for empty cell without @optional for the same rule" in {
      val schema =
        """version 1.0
           @totalColumns 4 @noHeader
           Col1:
           Col2: regex("[0-9]") @optional
           Col3: regex("[0-9]")
           Col4:
        """

      val metaData =
        """1,,,4
           1,a,,4
        """

      validate(metaData, schema, None) should beLike {
        case Failure(messages) => messages.list mustEqual List(
          ErrorMessage("""regex("[0-9]") fails for line: 1, column: Col3, value: """"",Some(1),Some(2)),
          ErrorMessage("""regex("[0-9]") fails for line: 2, column: Col2, value: "a"""",Some(2),Some(1)),
          ErrorMessage("""regex("[0-9]") fails for line: 2, column: Col3, value: """"",Some(2),Some(2)))
      }
    }

    "ignore case of a given regex" in {
      val schema =
        """version 1.0
           @totalColumns 1 @noHeader
           1: regex("[a-z]+") @ignoreCase
        """

      val metaData = """SCOOBY"""

      validate(metaData, schema, None) must beLike { case Success(_) => ok }
    }

    "ignore case with in rule succeeds if value contains regex characters" in {
      val schema =
        """version 1.0
           @totalColumns 1 @noHeader
           1: in("[abc]") @ignoreCase
        """

      val metaData ="""[Abc"""

      validate(metaData, schema, None) must beLike { case Success(_) => ok }
    }

    "fail to ignore case of a given regex when not providing @ignoreCase" in {
      val schema =
        """version 1.0
           @totalColumns 1 @noHeader
           Col1: regex("[a-z]+")
        """

      val metaData = "SCOOBY"

      validate(metaData, schema, None) should beLike {
        case Failure(messages) => messages.list mustEqual List(ErrorMessage("""regex("[a-z]+") fails for line: 1, column: Col1, value: "SCOOBY"""",Some(1),Some(0)))
      }
    }

    "succeed with valid file path" in {
      val schema =
        """version 1.0
           @totalColumns 1 @noHeader
           1: fileExists
        """

      val metaData = mustExistForRulePath

      validate(metaData, schema, None) must beLike { case Success(_) => ok }
    }

    "succeed with valid file path where fileExists rule prepends root path to the filename" in {

      val segments = mustExistForRulePath.split(FILE_SEPARATOR)
      val relPath = (segments.slice(0, segments.length - 2).reduceLeft(_ + FILE_SEPARATOR + _), segments.slice(segments.length - 2, segments.length).reduceLeft(_ + FILE_SEPARATOR + _))

      val schema =
        """version 1.0
           @totalColumns 1 @noHeader
           1: fileExists("""" + relPath._1 + """")
        """

      val metaData = relPath._2

      validate(metaData, schema, None) must beLike { case Success(_) => ok }
    }

    "fail for non existent file path" in {
      val schema =
        """version 1.0
           @totalColumns 1 @noHeader
           FirstColumn: fileExists
        """

      val metaData = "some/non/existent/file"

      validate(metaData, schema, None) must beLike {
        case Failure(messages) => messages.list mustEqual List(ErrorMessage("fileExists fails for line: 1, column: FirstColumn, value: \"some/non/existent/file\"",Some(1),Some(0)))
      }
    }

    "fail when first line contains invalid data and noHeader directive is set" in {
      val schema =
        """version 1.0
           @totalColumns 2 @noHeader
           col1:
           col2WithRule: regex("[0-9a-z]*") in("dog")
        """

      val metaData =
        """someData,thisisrubbish
           someMore,dog
        """

      validate(metaData, schema, None) must beLike {
        case Failure(messages) => messages.list mustEqual List(ErrorMessage("""in("dog") fails for line: 1, column: col2WithRule, value: "thisisrubbish"""",Some(1),Some(1)))
      }
    }

//    "fail when first line contains invalid data and noHeader directive is not set" in {
//      val schema =
//        """version 1.0
//           @totalColumns 2
//           col1:
//           col2WithRule: regex("[0-9a-z]*") in("dog")
//        """
//
//      val metaData =
//        """someData,this line is skipped because no header is not set i.e there is a header (this line) to skip
//           someMore,thisisrubbish
//        """
//
//      validate(metaData, schema, None) must beLike {
//        case Failure(messages) => messages.list mustEqual List(ErrorMessage("""in("dog") fails for line: 1, column: col2WithRule, value: "thisisrubbish"""",Some(1),Some(1)))
//      }
//    }

    "fail for completely empty metadata file when this is not permitted" in {
      val schema =
        """version 1.0
           @totalColumns 1
           @noHeader
           Col1:
        """

      val metaData = ""

      validate(metaData, schema, None) must beLike {
        case Failure(messages) => messages.list mustEqual List(ErrorMessage("metadata file is empty but this has not been permitted"))
      }
    }

    "succeed for completely empty metadata file when this is permitted" in {
      val schema =
        """version 1.0
           @totalColumns 1
           @noHeader
           @permitEmpty
           Col1:
        """

      val metaData = ""

      validate(metaData, schema, None) must beLike { case Success(_) => ok }
    }

    "fail for permitted empty metaData file when expecting a header" in {
      val schema =
        """version 1.0
           @totalColumns 1
           @permitEmpty
           Col1:
        """

      val metaData = ""

      validate(metaData, schema, None) must beLike {
        case Failure(messages) => messages.list mustEqual List(ErrorMessage("metadata file is empty but should contain at least a header"))
      }
    }

    "succeed for permitted empty metadata file with header only" in {
      val schema =
        """version 1.0
           @totalColumns 1
           @permitEmpty
           Name:
        """

      val metaData = "Name"

      validate(metaData, schema, None) must beLike { case Success(_) => ok }
    }

    "fail for metaData file with only a header line when not permitted empty" in {
      val schema =
        """version 1.0
           @totalColumns 1
           Name:
        """

      val metaData = "Name"

      validate(metaData, schema, None) must beLike {
        case Failure(messages) => messages.list mustEqual List(ErrorMessage("metadata file has a header but no data and this has not been permitted"))
      }
    }

    "succeed when either side of or rule passes" in {
      val schema =
        """version 1.0
           @totalColumns 1 @noHeader
           ThisOrThat: in("This") or in("That")
        """

      val metaData =
        """This
           That
        """

      validate(metaData, schema, None) must beLike { case Success(_) => ok }
    }

    "fail when neither side of or rule passes" in {
      val schema =
        """version 1.0
           @totalColumns 1 @noHeader
           ThisOrThat: in("This") or in("That")
        """

      val metaData = "SomethingElse"

      validate(metaData, schema, None) must beLike {
        case Failure(messages) => messages.list mustEqual List(ErrorMessage("""in("This") or in("That") fails for line: 1, column: ThisOrThat, value: "SomethingElse"""",Some(1),Some(0)))
      }
    }

    "succeed when one of 3 'or' rules passes" in {
      val schema =
        """version 1.0
           @totalColumns 1 @noHeader
           Red: regex("\\d+") or regex("R.*") or in("red")
        """

      val metaData =
        """Red
           red
        """

      validate(metaData, schema, None) must beLike { case Success(_) => ok }
    }

    "succeed for 'is' rule" in {
      val schema =
        """version 1.0
           @totalColumns 1 @noHeader
           Country: is("UK")
        """

      val metaData = "UK"

      validate(metaData, schema, None) must beLike { case Success(_) => ok }
    }

    "succeed for 'is' cross reference rule" in {
      val schema =
        """version 1.0
           @totalColumns 2 @noHeader
           Country: is($MyCountry)
           MyCountry:
        """

      val metaData = "United Kingdom,United Kingdom"

      validate(metaData, schema, None) must beLike { case Success(_) => ok }
    }

    "succeed for 2 'is' rule" in {
      val schema =
        """version 1.0
           @totalColumns 1 @noHeader
           Country: is("UK") is("uk") @ignoreCase
        """

      val metaData = "UK"

      validate(metaData, schema, None) must beLike { case Success(_) => ok }
    }

    "fail for 'is' rule that is not matched" in {
      val schema =
        """version 1.0
           @totalColumns 1 @noHeader
           Country: is("France")
        """

      val metaData = "UK"

      validate(metaData, schema, None) must beLike {
        case Failure(messages) => messages.list mustEqual List(ErrorMessage("""is("France") fails for line: 1, column: Country, value: "UK"""",Some(1),Some(0)))
      }
    }

    "fail for 'is' cross reference rule that is not matched" in {
      val schema =
        """version 1.0
           @totalColumns 2 @noHeader
           Country: is($MyCountry)
           MyCountry:
        """

      val metaData = "United,United Kingdom"

      validate(metaData, schema, None) must beLike {
        case Failure(messages) => messages.list mustEqual List(ErrorMessage("""is($MyCountry) fails for line: 1, column: Country, value: "United"""",Some(1),Some(0)))
      }
    }

    "succeed for 'not' rule" in {
      val schema =
        """version 1.0
           @totalColumns 1 @noHeader
           Country: not("United States")
        """

      val metaData = "United Kingdom"

      validate(metaData, schema, None) must beLike { case Success(_) => ok }
    }

    "succeed for 'not' cross reference rule" in {
      val schema =
        """version 1.0
           @totalColumns 2 @noHeader
           Country: not($MyCountry)
           MyCountry:
        """

      val metaData = "United Kingdom,United States"

      validate(metaData, schema, None) must beLike { case Success(_) => ok }
    }

    "succeed for 2 'not' rule" in {
      val schema =
        """version 1.0
           @totalColumns 1 @noHeader
           Country: not("United States") not("Kingdom") @ignoreCase
        """

      val metaData = "United Kingdom"

      validate(metaData, schema, None) must beLike { case Success(_) => ok }
    }

    "fail for 'not' rule that is not matched" in {
      val schema =
        """version 1.0
           @totalColumns 1 @noHeader
           Country: not("United Kingdom")
        """

      val metaData = "United Kingdom"

      validate(metaData, schema, None) must beLike {
        case Failure(messages) => messages.list mustEqual List(ErrorMessage("""not("United Kingdom") fails for line: 1, column: Country, value: "United Kingdom"""",Some(1),Some(0)))
      }
    }

    "fail for 'not' cross reference rule that is not matched" in {
      val schema =
        """version 1.0
           @totalColumns 2 @noHeader
           Country: not($MyCountry)
           MyCountry:
        """

      val metaData = "United Kingdom,United Kingdom"

      validate(metaData, schema, None) must beLike {
        case Failure(messages) => messages.list mustEqual List(ErrorMessage("""not($MyCountry) fails for line: 1, column: Country, value: "United Kingdom"""",Some(1),Some(0)))
      }
    }

    "succeed for 'starts' rule" in {
      val schema =
        """version 1.0
           @totalColumns 1 @noHeader
           Country: starts("United")
        """

      val metaData = "United Kingdom"

      validate(metaData, schema, None) must beLike { case Success(_) => ok }
    }

    "succeed for 'starts' cross reference rule" in {
      val schema =
        """version 1.0
           @totalColumns 2 @noHeader
           Country: starts($MyCountry)
           MyCountry:
        """

      val metaData = "United Kingdom,United"

      validate(metaData, schema, None) must beLike { case Success(_) => ok }
    }

    "succeed for 2 'starts' rule" in {
      val schema =
        """version 1.0
           @totalColumns 1 @noHeader
           Country: starts("United") starts("UNITED") @ignoreCase
        """

      val metaData = "United Kingdom"

      validate(metaData, schema, None) must beLike { case Success(_) => ok }
    }

    "fail for 'starts' rule that is not matched" in {
      val schema =
        """version 1.0
           @totalColumns 1 @noHeader
           Country: starts("united")
        """

      val metaData = "United Kingdom"

      validate(metaData, schema, None) must beLike {
        case Failure(messages) => messages.list mustEqual List(ErrorMessage("""starts("united") fails for line: 1, column: Country, value: "United Kingdom"""",Some(1),Some(0)))
      }
    }

    "fail for 'starts' cross reference rule that is not matched" in {
      val schema =
        """version 1.0
           @totalColumns 2 @noHeader
           Country: starts($MyCountry)
           MyCountry:
        """

      val metaData = "United,United Kingdom"

      validate(metaData, schema, None) must beLike {
        case Failure(messages) => messages.list mustEqual List(ErrorMessage("""starts($MyCountry) fails for line: 1, column: Country, value: "United"""",Some(1),Some(0)))
      }
    }

    "succeed for 'ends' rule" in {
      val schema =
        """version 1.0
           @totalColumns 1 @noHeader
           Country: ends("Kingdom")
        """

      val metaData = "United Kingdom"

      validate(metaData, schema, None) must beLike { case Success(_) => ok }
    }

    "succeed for 'ends' cross reference rule" in {
      val schema =
        """version 1.0
           @totalColumns 2 @noHeader
           Country: ends($MyCountry)
           MyCountry:
        """

      val metaData = "United Kingdom,Kingdom"

      validate(metaData, schema, None) must beLike { case Success(_) => ok }
    }

    "succeed for 2 'ends' rule" in {
      val schema =
        """version 1.0
           @totalColumns 1 @noHeader
           Country: ends("Kingdom") ends("KINGDOM") @ignoreCase
        """

      val metaData = "United Kingdom"

      validate(metaData, schema, None) must beLike { case Success(_) => ok }
    }

    "fail for 'ends' rule that is not matched" in {
      val schema =
        """version 1.0
           @totalColumns 1 @noHeader
           Country: ends("kingdom")"""

      val metaData = "United Kingdom"

      validate(metaData, schema, None) must beLike {
        case Failure(messages) => messages.list mustEqual List(ErrorMessage("""ends("kingdom") fails for line: 1, column: Country, value: "United Kingdom"""",Some(1),Some(0)))
      }
    }

    "fail for 'ends' cross reference rule that is not matched" in {
      val schema =
        """version 1.0
           @totalColumns 2 @noHeader
           Country: ends($MyCountry)
           MyCountry:
        """

      val metaData = "United Kingdom,States"

      validate(metaData, schema, None) must beLike {
        case Failure(messages) => messages.list mustEqual List(ErrorMessage("""ends($MyCountry) fails for line: 1, column: Country, value: "United Kingdom"""",Some(1),Some(0)))
      }
    }

    "succeed for unique rule that is unique" in {
      val schema = """version 1.0
                      @totalColumns 2 @noHeader
                      Name: unique
                      Age: range(0,100)"""

      val metaData =
        """Bob,10
           Jim,20
           Ben,30
           David,40
           Andy,50
        """

      validate(metaData, schema, None) must beLike { case Success(_) => ok }
    }

    "fail for multiple duplicates for unique rule" in {

      val schema =
        """version 1.0
           @totalColumns 1 @noHeader
           Name: unique
        """

      val metaData =
        """Bob
           Jim
           Ben
           Jim
           Jim
        """

      validate(metaData, schema, None) must beLike {
        case Failure(messages) => messages.list mustEqual List(ErrorMessage("unique fails for line: 4, column: Name, value: \"Jim\" (original at line: 2)",Some(4),Some(0)),ErrorMessage("unique fails for line: 5, column: Name, value: \"Jim\" (original at line: 2)",Some(5),Some(0)))
      }
    }

    "succeed for multi-column unique rule" in {
      val schema = """version 1.0
                      @totalColumns 3 @noHeader
                      Name: unique($Age,$PostCode)
                      Age:
                      PostCode:
                   """

      val metaData =
        """Bob,10,N1
           Jim,20,TN20
           Bob,50,CR3
           Ben,30,SE1
           David,40,B5
           Andy,50,ED9
        """

      validate(metaData, schema, None) must beLike { case Success(_) => ok }
    }



    "succeed for valid uri" in {
      val schema = """version 1.0
                      @totalColumns 1 @noHeader
                      uri: uri"""

      val metaData =
        """http://datagov.nationalarchives.gov.uk/66/WO/409/123/123/12345678-1234-4abc-8123-ffffffffffff
           http://datagov.nationalarchives.gov.uk/66/WO/409/456/222/aaaaaaaa-1234-4abc-9132-aaaaaaaaaaaa
           http://datagov.nationalarchives.gov.uk/66/WO/409/789/22222/aa11bb33-1234-4abc-a123-ffffffffffff
           http://datagov.nationalarchives.gov.uk/66/WO/409/1234/4444/12345678-5678-4abc-b123-aaaaaaaaaaaa
           http://datagov.nationalarchives.gov.uk/66/WO/409/5678/1333/12345678-abcd-4abc-8123-ffffffffffff
        """

      validate(metaData, schema, None) must beLike { case Success(_) => ok }
    }

    "succeed for valid xDateTime" in {
      val schema = """version 1.0
                      @totalColumns 1 @noHeader
                      date: xDateTime"""

      val metaData =
        """1959-06-20T12:59:59.000
           1959-06-20T12:59:59.999
           1969-07-21T02:56:00.456
           1215-06-15T00:00:00+23:59
           1844-05-24T00:00:00-23:30
           1066-10-14T00:00:00-01:00
           1966-07-30T00:00:00
           1966-07-30T00:00:00Z
           1966-07-30T00:00:00+01:00
        """

      validate(metaData, schema, None) must beLike { case Success(_) => ok }
    }

    "succeed for valid xDateTime range" in {
      val schema = """version 1.0
                      @totalColumns 1 @noHeader
                      date: xDateTime(1959-06-20T12:59:59.000-04:00, 1969-07-21T02:56:00.456+07:00)"""

      val metaData =
        """1959-06-29T12:59:59.000-04:00
           1959-06-20T18:00:00.000Z
           1963-06-20T12:00:00
           1969-07-21T02:56:00.456+07:00
        """
      validate(metaData, schema, None) must beLike { case Success(_) => ok }
    }

    "succeed for valid xDate" in {
      val schema = """version 1.0
                      @totalColumns 1 @noHeader
                      date: xDate"""

      val metaData =
        """1953-06-02
           1945-09-02
           2001-09-11
           1805-10-21
           1963-11-22
        """

      validate(metaData, schema, None) must beLike { case Success(_) => ok }
    }

    "succeed for valid UK Date" in {
      val schema = """version 1.0
                      @totalColumns 1 @noHeader
                      date: ukDate"""

      val metaData =
        """02/02/2013
           02/02/2013
           03/12/2013
           12/12/2013
           12/12/2013
        """

      validate(metaData, schema, None) must beLike { case Success(_) => ok }
    }


    "succeed for valid Part UK Date" in {
      val schema = """version 1.0
                      @totalColumns 1 @noHeader
                      date: partUkDate"""

      val metaData =
        """02/February/2013
           0?/February/2013
           ??/May/2013
           02/*/2013
           */?/*
           12/?/2013
        """
//  */  -- to keep intellij happy (Not a scala error)

      validate(metaData, schema, None) must beLike { case Success(_) => ok }
    }

    "succeed for valid Xsd Time" in {
      val schema = """version 1.0
                      @totalColumns 1 @noHeader
                      date: xTime"""

      val metaData =
        """09:11:10"""

      validate(metaData, schema, None) must beLike { case Success(_) => ok }
    }

    "succeed for valid uuid 4 rule" in {
      val schema = """version 1.0
                      @totalColumns 1 @noHeader
                      date: uuid4"""

      val metaData =
        """12345678-1234-4abc-8123-ffffffffffff
           aaaaaaaa-1234-4abc-9132-aaaaaaaaaaaa
           aa11bb33-1234-4abc-a123-ffffffffffff
        """

      validate(metaData, schema, None) must beLike { case Success(_) => ok }
    }

    "succeed for valid positive integer" in {
      val schema = """version 1.0
                      @totalColumns 1 @noHeader
                      date: positiveInteger"""

      val metaData =
        """001
           12340
           123456789
        """

      validate(metaData, schema, None) must beLike { case Success(_) => ok }
    }


    "succeed for valid ranges" in {
      val schema = """version 1.0
                      @totalColumns 2  @noHeader
                      Name:
                      Age: range(0,100)"""

      val metaData =
        """Bob,10
           Jim,20
           Ben,30
           Jim,40
           Jim,50
        """

      validate(metaData, schema, None) must beLike { case Success(_) => ok }
    }

    "fail when values outside range" in {
      val schema = """version 1.0
                      @totalColumns 2 @noHeader
                      Name:
                      Age: range(18,65)"""

      val metaData =
        """Bob,10
           Jim,21
           Ben,96
           Jim,40
           Jim,50
        """

      validate(metaData, schema, None) must beLike {
        case Failure(messages) => messages.list mustEqual List(ErrorMessage("range(18,65) fails for line: 1, column: Age, value: \"10\"",Some(1),Some(1)),ErrorMessage("range(18,65) fails for line: 3, column: Age, value: \"96\"",Some(3),Some(1)))
      }
    }

    "fail when cell length outside range" in {
      val schema = """version 1.0
                      @totalColumns 1 @noHeader
                      Name: length(*,3)"""

      val metaData =
        """Bob
           Jim
           Benny
           Jim
           Timmy
        """

      validate(metaData, schema, None) must beLike {
        case Failure(messages) => messages.list mustEqual List(ErrorMessage("length(*,3) fails for line: 3, column: Name, value: \"Benny\"",Some(3),Some(0)),ErrorMessage("length(*,3) fails for line: 5, column: Name, value: \"Timmy\"",Some(5),Some(0)))
      }
    }

    "succeed with an 'and' command" in {
      val schema = """version 1.0
                      @totalColumns 1 @noHeader
                      Name: length(5) and length(*,*) and is("Hello")"""

      val metaData =
        """Hello
        """

      validate(metaData, schema, None) must beLike { case Success(_) => ok }
    }



    "fail with good () error message" in {
      val schema = """version 1.0
                      @totalColumns 1 @noHeader
                      Name: (length(5) and length(*,*) ) and is("Hello")"""

      val metaData =
        """Hello
           World
        """

      validate(metaData, schema, None) must beLike {
        case Failure(messages) => messages.list mustEqual List(ErrorMessage("""(length(5) and length(*,*)) and is("Hello") fails for line: 2, column: Name, value: "World"""",Some(2),Some(0)))
      }
    }

    "succeed when checksum matches given value" in {
      val schema =
        """version 1.0
           @totalColumns 2 @noHeader
           File:
           MD5: checksum(file("""" + checksumPath + """"), "MD5")
        """

      val metaData = s"$checksumPath,232762380299115da6995e4c4ac22fa2"

      validate(metaData, schema, None) must beLike { case Success(_) => ok }
    }

    "succeed when fileCount matches given root & cross referenced file" in {
      val schema =
        """version 1.0
           @totalColumns 2 @noHeader
           File:
           Count: fileCount(file("""" + schemaPath + """", $File))
        """

      val metaData = """checksum.csvs,1"""

      validate(metaData, schema, None) must beLike { case Success(_) => ok }
    }

    "succeed column id with A-Z a-z 0-9 - _ ." in {
      val schema =
        """version 1.0
           @totalColumns 1 @noHeader
           Na123-me._:
        """
      val metaData ="Joe Bloggs"
      validate(metaData, schema, None) must beLike { case Success(_) => ok }
    }

    "succeed for simple if where condition is not true" in {
      val schema =
        """version 1.0
           @totalColumns 1 @noHeader
           col1: if(starts("Jim"), ends("ogsf") )
        """
      val metaData ="Joe Bloggs"
      validate(metaData, schema, None) must beLike { case Success(_) => ok }
    }

    "succeed for simple if with combinator" in {
      val schema =
        """version 1.0
           @totalColumns 1 @noHeader
           col1: if(starts("Tom") or starts("Joe"), ends("Bloggs") )
        """
      val metaData ="Joe Bloggs"
      validate(metaData, schema, None) must beLike { case Success(_) => ok }
    }

    "succeed for simple if where condition is true" in {
      val schema =
        """version 1.0
           @totalColumns 1 @noHeader
           col1: if(starts("Joe"), ends("oggs") )
        """
      val metaData ="Joe Bloggs"
      validate(metaData, schema, None) must beLike { case Success(_) => ok }
    }

    "fail for simple if where condition is true and expr is false" in {
      val schema =
        """version 1.0
           @totalColumns 1 @noHeader
           col1: if(starts("Joe"), ends("Joe") )
        """
      val metaData ="Joe Bloggs"
      validate(metaData, schema, None) must beLike {
        case Failure(messages) => messages.list mustEqual List(ErrorMessage("""ends("Joe") fails for line: 1, column: col1, value: "Joe Bloggs"""",Some(1),Some(0)))
      }
    }

    "succeed for simple if where condition is false and else expr is true" in {
      val schema =
        """version 1.0
           @totalColumns 1 @noHeader
           col1: if(starts("Jim"), ends("Joe") , ends("oggs")  )
        """
      val metaData ="Joe Bloggs"
      validate(metaData, schema, None) must beLike { case Success(_) => ok }
    }

    "fail for simple if where condition is false and else expr is false" in {
      val schema =
        """version 1.0
           @totalColumns 1 @noHeader
           col1: if(starts("ogs"), ends("oggs") , ends("Joe"))
        """
      val metaData ="Joe Bloggs"
      validate(metaData, schema, None) must beLike {
        case Failure(messages) => messages.list mustEqual List(ErrorMessage("""ends("Joe") fails for line: 1, column: col1, value: "Joe Bloggs"""",Some(1),Some(0)))
      }
    }

    "succeed for simple if where condition is true and else expr is false and expr is true" in {
      val schema =
        """version 1.0
           @totalColumns 1 @noHeader
           col1: if(starts("Joe"), ends("oggs") , ends("Joe"))
        """
      val metaData ="Joe Bloggs"
      validate(metaData, schema, None) must beLike { case Success(_) => ok }
    }

    "succeed for nested if where condition is true" in {
      val schema =
        """version 1.0
           @totalColumns 1 @noHeader
           col1: if(starts("Joe"), if( starts("Bloggs"), length(4), length(1, 20) ) )
        """
      val metaData ="Joe Bloggs"
      validate(metaData, schema, None) must beLike { case Success(_) => ok }
    }

    "fail for nested if where condition is true" in {
      val schema =
        """version 1.0
           @totalColumns 1 @noHeader
           col1: if(starts("Joe"), if( starts("Joe"), length(4), length(1, 20) ) )
        """
      val metaData ="Joe Bloggs"
      validate(metaData, schema, None) must beLike {
        case Failure(messages) => messages.list mustEqual List(ErrorMessage("""length(4) fails for line: 1, column: col1, value: "Joe Bloggs"""",Some(1),Some(0)))
      }
    }

    "succeed for nested if where condition is false" in {
      val schema =
        """version 1.0
           @totalColumns 1 @noHeader
           col1: if(starts("Bloggs"), if( starts("Joe"), length(4), length(1, 20) ) , if( starts("Joe"), length(4, 20), length(4) ) )
        """
      val metaData ="Joe Bloggs"
      validate(metaData, schema, None) must beLike { case Success(_) => ok }
    }


    "succeed when 'and'ing rules together" in {
      val schema =
        """version 1.0
           @totalColumns 1 @noHeader
           col1: starts("Dath") and ends("Vader")
        """
      val metaData ="Dath Vader"
      validate(metaData, schema, None) must beLike { case Success(_) => ok }
    }

    "succeed with parentheses creating a match, same as below with () difference" in {
      val schema =
        """version 1.0
           @totalColumns 1 @noHeader
           col1: is("True") or (is("True") and is("False"))
        """
      val metaData ="True"
      validate(metaData, schema, None) must beLike { case Success(_) => ok }
    }

    "fail with parentheses creating a failed match, same example as above, only () difference" in {
      val schema =
        """version 1.0
           @totalColumns 1 @noHeader
           col1: (is("True") or is("True")) and is("False")
        """
      val metaData ="True"
      validate(metaData, schema, None) must beLike {
        case Failure(messages) => messages.list mustEqual List(ErrorMessage("""(is("True") or is("True")) and is("False") fails for line: 1, column: col1, value: "True"""",Some(1),Some(0)))
      }
    }


    "succeed with an explicit column reference" in {
      val schema =
        """version 1.0
           @totalColumns 2 @noHeader
           Name: $Age/is("900") or $Age/is("25")
           Age:  if( $Name/is("Yoda"), is("900"))
        """

      val metaData =
        """Bob,25
           Yoda,900
        """

      validate(metaData, schema, None) must beLike { case Success(_) => ok }
    }

  }
}