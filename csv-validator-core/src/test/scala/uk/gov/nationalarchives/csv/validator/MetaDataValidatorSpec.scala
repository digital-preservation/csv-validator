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
      override def parse(reader: Reader): ParseResult[Schema] = {
        super.parse(reader) match {
          case s @ Success(schema: Schema, _) => s
          case NoSuccess(message, next) => throw new RuntimeException(message)
        }
      }
    }

    schemaParser.parse(s).get
  }

  object TestMetaDataValidator extends AllErrorsMetaDataValidator { val pathSubstitutions = List[(String,String)]() }

  import TestMetaDataValidator._

  val mustExistForRulePath = resourcePath("schema/mustExistForRule.txt")

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

      validate(metaData, schema) must beLike { case Success(_) => ok }
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

      validate(metaData, schema) must beLike {
        case Failure(messages) => messages.list mustEqual List(ErrorMessage("Expected @totalColumns of 3 and found 2 on line 2"), ErrorMessage("Missing value at line: 2, column: column3"))
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

      validate(metaData, schema) must beLike {
        case Failure(messages) => messages.list mustEqual List (
          ErrorMessage("""regex("[a-c]*") fails for line: 1, column: second, value: "xxxy""""),
          ErrorMessage("""regex("[3-8]*") fails for line: 2, column: first, value: "abcd""""),
          ErrorMessage("""regex("[a-c]*") fails for line: 2, column: second, value: "uii""""))
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

      validate(metaData, schema) must beLike { case Success(_) => ok }
    }

    "fail for a single rule" in {
      val schema =
        """version 1.0
           @totalColumns 2 @noHeader
           Col1: regex("C11")
           Col2:
        """

      val metaData = "c11,c12"

      validate(metaData, schema) should beLike {
        case Failure(messages) => messages.list mustEqual List(ErrorMessage("""regex("C11") fails for line: 1, column: Col1, value: "c11""""))
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

      validate(metaData, schema) should beLike {
        case Failure(messages) => messages.list mustEqual List(ErrorMessage("Expected @totalColumns of 2 and found 1 on line 1"), ErrorMessage("Missing value at line: 1, column: Col2"))
      }
    }

    "succeed for more than one regex" in {
      val schema =
        """version 1.0
           @totalColumns 1 @noHeader
           c1: regex("\w+") regex("^S.+")
        """

      val metaData = """Scooby"""

      validate(metaData, schema) must beLike { case Success(_) => ok }
    }

    "fail when at least one regex fails for multiple regex provided in a column definition" in {
      val schema =
        """version 1.0
           @totalColumns 1 @noHeader
           c1: regex("\w+") regex("^T.+") regex("^X.+")
        """

      val metaData = """Scooby"""

      validate(metaData, schema) must beLike {
        case Failure(messages) => messages.list mustEqual List(ErrorMessage("""regex("^T.+") fails for line: 1, column: c1, value: "Scooby""""), ErrorMessage("""regex("^X.+") fails for line: 1, column: c1, value: "Scooby""""))
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

      validate(metaData, schema) must beLike { case Success(_) => ok }
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
      validate(metaData, schema) must beLike { case Success(_) => ok }
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

      validate(metaData, schema) should beLike {
        case Failure(messages) => messages.list mustEqual List(ErrorMessage("""in($col1) fails for line: 1, column: col2WithRule, value: "mustBeIn""""))
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

      validate(metaData, schema) must beLike { case Success(_) => ok }
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

      validate(metaData, schema) must beLike { case Success(_) => ok }
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

      validate(metaData, schema) should beLike {
        case Failure(messages) => messages.list mustEqual List(ErrorMessage("""regex("[0-9]") fails for line: 1, column: Col2, value: "a""""))
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

      validate(metaData, schema) should beLike {
        case Failure(messages) => messages.list mustEqual List(
          ErrorMessage("""regex("[0-9]") fails for line: 1, column: Col3, value: """""),
          ErrorMessage("""regex("[0-9]") fails for line: 2, column: Col2, value: "a""""),
          ErrorMessage("""regex("[0-9]") fails for line: 2, column: Col3, value: """""))
      }
    }

    "ignore case of a given regex" in {
      val schema =
        """version 1.0
           @totalColumns 1 @noHeader
           1: regex("[a-z]+") @ignoreCase
        """

      val metaData = """SCOOBY"""

      validate(metaData, schema) must beLike { case Success(_) => ok }
    }

    "ignore case with in rule succeeds if value contains regex characters" in {
      val schema =
        """version 1.0
           @totalColumns 1 @noHeader
           1: in("[abc]") @ignoreCase
        """

      val metaData ="""[Abc"""

      validate(metaData, schema) must beLike { case Success(_) => ok }
    }

    "fail to ignore case of a given regex when not providing @ignoreCase" in {
      val schema =
        """version 1.0
           @totalColumns 1 @noHeader
           Col1: regex("[a-z]+")
        """

      val metaData = "SCOOBY"

      validate(metaData, schema) should beLike {
        case Failure(messages) => messages.list mustEqual List(ErrorMessage("""regex("[a-z]+") fails for line: 1, column: Col1, value: "SCOOBY""""))
      }
    }

    "succeed with valid file path" in {
      val schema =
        """version 1.0
           @totalColumns 1 @noHeader
           1: fileExists
        """

      val metaData = mustExistForRulePath

      validate(metaData, schema) must beLike { case Success(_) => ok }
    }

    "succeed with valid file path where fileExists rule prepends root path to the filename" in {

      val segments = mustExistForRulePath.split('/')
      val relPath = (segments.slice(0, segments.length - 2).reduceLeft(_ + '/' + _), segments.slice(segments.length - 2, segments.length).reduceLeft(_ + '/' + _))

      val schema =
        """version 1.0
           @totalColumns 1 @noHeader
           1: fileExists("""" + relPath._1 + """")
        """

      val metaData = relPath._2

      validate(metaData, schema) must beLike { case Success(_) => ok }
    }

    "fail for non existent file path" in {
      val schema =
        """version 1.0
           @totalColumns 1 @noHeader
           FirstColumn: fileExists
        """

      val metaData = "some/non/existent/file"

      validate(metaData, schema) must beLike {
        case Failure(messages) => messages.list mustEqual List(ErrorMessage("fileExists fails for line: 1, column: FirstColumn, value: \"some/non/existent/file\""))
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

      validate(metaData, schema) must beLike {
        case Failure(messages) => messages.list mustEqual List(ErrorMessage("""in("dog") fails for line: 1, column: col2WithRule, value: "thisisrubbish""""))
      }
    }

    "fail when first line contains invalid data and noHeader directive is not set" in {
      val schema =
        """version 1.0
           @totalColumns 2
           col1:
           col2WithRule: regex("[0-9a-z]*") in("dog")
        """

      val metaData =
        """someData,this line is skipped because no header is not set i.e there is a header (this line) to skip
           someMore,thisisrubbish
        """

      validate(metaData, schema) must beLike {
        case Failure(messages) => messages.list mustEqual List(ErrorMessage("""in("dog") fails for line: 1, column: col2WithRule, value: "thisisrubbish""""))
      }
    }

    "fail for empty metaData file when expecting header" in {
      val schema =
        """version 1.0
           @totalColumns 1
           Col1:
        """

      val metaData = ""

      validate(metaData, schema) must beLike {
        case Failure(messages) => messages.list mustEqual List(ErrorMessage("metadata file is empty"))
      }

    }

    "fail for metaData file with only a header line" in {
      val schema =
        """version 1.0
           @totalColumns 1
           Col1:
        """

      val metaData = "Name"

      validate(metaData, schema) must beLike {
        case Failure(messages) => messages.list mustEqual List(ErrorMessage("metadata file has a header but no data"))
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

      validate(metaData, schema) must beLike { case Success(_) => ok }
    }

    "fail when neither side of or rule passes" in {
      val schema =
        """version 1.0
           @totalColumns 1 @noHeader
           ThisOrThat: in("This") or in("That")
        """

      val metaData = "SomethingElse"

      validate(metaData, schema) must beLike {
        case Failure(messages) => messages.list mustEqual List(ErrorMessage("""in("This") or in("That") fails for line: 1, column: ThisOrThat, value: "SomethingElse""""))
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

      validate(metaData, schema) must beLike { case Success(_) => ok }
    }

    "succeed for 'is' rule" in {
      val schema =
        """version 1.0
           @totalColumns 1 @noHeader
           Country: is("UK")
        """

      val metaData = "UK"

      validate(metaData, schema) must beLike { case Success(_) => ok }
    }

    "succeed for 'is' cross reference rule" in {
      val schema =
        """version 1.0
           @totalColumns 2 @noHeader
           Country: is($MyCountry)
           MyCountry:
        """

      val metaData = "United Kingdom,United Kingdom"

      validate(metaData, schema) must beLike { case Success(_) => ok }
    }

    "succeed for 2 'is' rule" in {
      val schema =
        """version 1.0
           @totalColumns 1 @noHeader
           Country: is("UK") is("uk") @ignoreCase
        """

      val metaData = "UK"

      validate(metaData, schema) must beLike { case Success(_) => ok }
    }

    "fail for 'is' rule that is not matched" in {
      val schema =
        """version 1.0
           @totalColumns 1 @noHeader
           Country: is("France")
        """

      val metaData = "UK"

      validate(metaData, schema) must beLike {
        case Failure(messages) => messages.list mustEqual List(ErrorMessage("""is("France") fails for line: 1, column: Country, value: "UK""""))
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

      validate(metaData, schema) must beLike {
        case Failure(messages) => messages.list mustEqual List(ErrorMessage("""is($MyCountry) fails for line: 1, column: Country, value: "United""""))
      }
    }

    "succeed for 'isNot' rule" in {
      val schema =
        """version 1.0
           @totalColumns 1 @noHeader
           Country: isNot("United States")
        """

      val metaData = "United Kingdom"

      validate(metaData, schema) must beLike { case Success(_) => ok }
    }

    "succeed for 'isNot' cross reference rule" in {
      val schema =
        """version 1.0
           @totalColumns 2 @noHeader
           Country: isNot($MyCountry)
           MyCountry:
        """

      val metaData = "United Kingdom,United States"

      validate(metaData, schema) must beLike { case Success(_) => ok }
    }

    "succeed for 2 'isNot' rule" in {
      val schema =
        """version 1.0
           @totalColumns 1 @noHeader
           Country: isNot("United States") isNot("Kingdom") @ignoreCase
        """

      val metaData = "United Kingdom"

      validate(metaData, schema) must beLike { case Success(_) => ok }
    }

    "fail for 'isNot' rule that is not matched" in {
      val schema =
        """version 1.0
           @totalColumns 1 @noHeader
           Country: isNot("United Kingdom")
        """

      val metaData = "United Kingdom"

      validate(metaData, schema) must beLike {
        case Failure(messages) => messages.list mustEqual List(ErrorMessage("""isNot("United Kingdom") fails for line: 1, column: Country, value: "United Kingdom""""))
      }
    }

    "fail for 'isNot' cross reference rule that is not matched" in {
      val schema =
        """version 1.0
           @totalColumns 2 @noHeader
           Country: isNot($MyCountry)
           MyCountry:
        """

      val metaData = "United Kingdom,United Kingdom"

      validate(metaData, schema) must beLike {
        case Failure(messages) => messages.list mustEqual List(ErrorMessage("""isNot($MyCountry) fails for line: 1, column: Country, value: "United Kingdom""""))
      }
    }

    "succeed for 'starts' rule" in {
      val schema =
        """version 1.0
           @totalColumns 1 @noHeader
           Country: starts("United")
        """

      val metaData = "United Kingdom"

      validate(metaData, schema) must beLike { case Success(_) => ok }
    }

    "succeed for 'starts' cross reference rule" in {
      val schema =
        """version 1.0
           @totalColumns 2 @noHeader
           Country: starts($MyCountry)
           MyCountry:
        """

      val metaData = "United Kingdom,United"

      validate(metaData, schema) must beLike { case Success(_) => ok }
    }

    "succeed for 2 'starts' rule" in {
      val schema =
        """version 1.0
           @totalColumns 1 @noHeader
           Country: starts("United") starts("UNITED") @ignoreCase
        """

      val metaData = "United Kingdom"

      validate(metaData, schema) must beLike { case Success(_) => ok }
    }

    "fail for 'starts' rule that is not matched" in {
      val schema =
        """version 1.0
           @totalColumns 1 @noHeader
           Country: starts("united")
        """

      val metaData = "United Kingdom"

      validate(metaData, schema) must beLike {
        case Failure(messages) => messages.list mustEqual List(ErrorMessage("""starts("united") fails for line: 1, column: Country, value: "United Kingdom""""))
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

      validate(metaData, schema) must beLike {
        case Failure(messages) => messages.list mustEqual List(ErrorMessage("""starts($MyCountry) fails for line: 1, column: Country, value: "United""""))
      }
    }

    "succeed for 'ends' rule" in {
      val schema =
        """version 1.0
           @totalColumns 1 @noHeader
           Country: ends("Kingdom")
        """

      val metaData = "United Kingdom"

      validate(metaData, schema) must beLike { case Success(_) => ok }
    }

    "succeed for 'ends' cross reference rule" in {
      val schema =
        """version 1.0
           @totalColumns 2 @noHeader
           Country: ends($MyCountry)
           MyCountry:
        """

      val metaData = "United Kingdom,Kingdom"

      validate(metaData, schema) must beLike { case Success(_) => ok }
    }

    "succeed for 2 'ends' rule" in {
      val schema =
        """version 1.0
           @totalColumns 1 @noHeader
           Country: ends("Kingdom") ends("KINGDOM") @ignoreCase
        """

      val metaData = "United Kingdom"

      validate(metaData, schema) must beLike { case Success(_) => ok }
    }

    "fail for 'ends' rule that is not matched" in {
      val schema =
        """version 1.0
           @totalColumns 1 @noHeader
           Country: ends("kingdom")"""

      val metaData = "United Kingdom"

      validate(metaData, schema) must beLike {
        case Failure(messages) => messages.list mustEqual List(ErrorMessage("""ends("kingdom") fails for line: 1, column: Country, value: "United Kingdom""""))
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

      validate(metaData, schema) must beLike {
        case Failure(messages) => messages.list mustEqual List(ErrorMessage("""ends($MyCountry) fails for line: 1, column: Country, value: "United Kingdom""""))
      }
    }

    "succeed for unique rule that is unique" in {
      val schema = """version 1.0
                      @totalColumns 2
                      Name: unique
                      Age: range(0,100)"""

      val metaData =
        """Bob,10
           Jim,20
           Ben,30
           David,40
           Andy,50
        """

      validate(metaData, schema) must beLike { case Success(_) => ok }
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

      validate(metaData, schema) must beLike {
        case Failure(messages) => messages.list mustEqual List(ErrorMessage("unique fails for line: 4, column: Name, value: \"Jim\" (original at line: 2)"),ErrorMessage("unique fails for line: 5, column: Name, value: \"Jim\" (original at line: 2)"))
      }
    }

    "succeed for multi-column unique rule" in {
      val schema = """version 1.0
                      @totalColumns 3
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

      validate(metaData, schema) must beLike { case Success(_) => ok }
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

      validate(metaData, schema) must beLike { case Success(_) => ok }
    }

    "succeed for valid xDateTime (if you can guess the dates without google see me for a prize)" in {
      val schema = """version 1.0
                      @totalColumns 1 @noHeader
                      date: xDateTime"""

      val metaData =
        """1969-07-21T02:56:00
           1215-06-15T00:00:00
           1844-05-24T00:00:00
           1066-10-14T00:00:00
           1966-07-30T00:00:00
        """

      validate(metaData, schema) must beLike { case Success(_) => ok }
    }

    "succeed for valid xDate (no prizes for the dates this time)" in {
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

      validate(metaData, schema) must beLike { case Success(_) => ok }
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

      validate(metaData, schema) must beLike { case Success(_) => ok }
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

      validate(metaData, schema) must beLike { case Success(_) => ok }
    }

    "succeed for valid Xsd Time" in {
      val schema = """version 1.0
                      @totalColumns 1 @noHeader
                      date: xTime"""

      val metaData =
        """09:11:10"""

      validate(metaData, schema) must beLike { case Success(_) => ok }
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

      validate(metaData, schema) must beLike { case Success(_) => ok }
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

      validate(metaData, schema) must beLike { case Success(_) => ok }
    }


    "succeed for valid ranges" in {
      val schema = """version 1.0
                      @totalColumns 2
                      Name:
                      Age: range(0,100)"""

      val metaData =
        """Bob,10
           Jim,20
           Ben,30
           Jim,40
           Jim,50
        """

      validate(metaData, schema) must beLike { case Success(_) => ok }
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

      validate(metaData, schema) must beLike {
        case Failure(messages) => messages.list mustEqual List(ErrorMessage("range(18,65) fails for line: 1, column: Age, value: \"10\""),ErrorMessage("range(18,65) fails for line: 3, column: Age, value: \"96\""))
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

      validate(metaData, schema) must beLike {
        case Failure(messages) => messages.list mustEqual List(ErrorMessage("length(*,3) fails for line: 3, column: Name, value: \"Benny\""),ErrorMessage("length(*,3) fails for line: 5, column: Name, value: \"Timmy\""))
      }
    }

    "succeed with an 'and' command" in {
      val schema = """version 1.0
                      @totalColumns 1 @noHeader
                      Name: length(5) and length(*,*) and is("Hello")"""

      val metaData =
        """Hello
        """

      validate(metaData, schema) must beLike { case Success(_) => ok }
    }



    "fail with good () error message" in {
      val schema = """version 1.0
                      @totalColumns 1 @noHeader
                      Name: (length(5) and length(*,*) ) and is("Hello")"""

      val metaData =
        """Hello
           World
        """

      validate(metaData, schema) must beLike {
        case Failure(messages) => messages.list mustEqual List(ErrorMessage("""(length(5) and length(*,*)) and is("Hello") fails for line: 2, column: Name, value: "World""""))
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

      validate(metaData, schema) must beLike { case Success(_) => ok }
    }

    "succeed when fileCount matches given root & cross referenced file" in {
      val schema =
        """version 1.0
           @totalColumns 2 @noHeader
           File:
           Count: fileCount(file("""" + schemaPath + """", $File))
        """

      val metaData = """checksum.txt,1"""

      validate(metaData, schema) must beLike { case Success(_) => ok }
    }

    "succeed column id with A-Z a-z 0-9 - _ ." in {
      val schema =
        """version 1.0
           @totalColumns 1 @noHeader
           Na123-me._:
        """
      val metaData ="Joe Bloggs"
      validate(metaData, schema) must beLike { case Success(_) => ok }
    }

    "succeed for simple if where condition is not true" in {
      val schema =
        """version 1.0
           @totalColumns 1 @noHeader
           col1: if(starts("Jim"), ends("ogsf") )
        """
      val metaData ="Joe Bloggs"
      validate(metaData, schema) must beLike { case Success(_) => ok }
    }

    "succeed for simple if with combinator" in {
      val schema =
        """version 1.0
           @totalColumns 1 @noHeader
           col1: if(starts("Tom") or starts("Joe"), ends("Bloggs") )
        """
      val metaData ="Joe Bloggs"
      validate(metaData, schema) must beLike { case Success(_) => ok }
    }

    "succeed for simple if where condition is true" in {
      val schema =
        """version 1.0
           @totalColumns 1 @noHeader
           col1: if(starts("Joe"), ends("oggs") )
        """
      val metaData ="Joe Bloggs"
      validate(metaData, schema) must beLike { case Success(_) => ok }
    }

    "fail for simple if where condition is true and expr is false" in {
      val schema =
        """version 1.0
           @totalColumns 1 @noHeader
           col1: if(starts("Joe"), ends("Joe") )
        """
      val metaData ="Joe Bloggs"
      validate(metaData, schema) must beLike {
        case Failure(messages) => messages.list mustEqual List(ErrorMessage("""ends("Joe") fails for line: 1, column: col1, value: "Joe Bloggs""""))
      }
    }

    "succeed for simple if where condition is false and else expr is true" in {
      val schema =
        """version 1.0
           @totalColumns 1 @noHeader
           col1: if(starts("Jim"), ends("Joe") , ends("oggs")  )
        """
      val metaData ="Joe Bloggs"
      validate(metaData, schema) must beLike { case Success(_) => ok }
    }

    "fail for simple if where condition is false and else expr is false" in {
      val schema =
        """version 1.0
           @totalColumns 1 @noHeader
           col1: if(starts("ogs"), ends("oggs") , ends("Joe"))
        """
      val metaData ="Joe Bloggs"
      validate(metaData, schema) must beLike {
        case Failure(messages) => messages.list mustEqual List(ErrorMessage("""ends("Joe") fails for line: 1, column: col1, value: "Joe Bloggs""""))
      }
    }

    "succeed for simple if where condition is true and else expr is false and expr is true" in {
      val schema =
        """version 1.0
           @totalColumns 1 @noHeader
           col1: if(starts("Joe"), ends("oggs") , ends("Joe"))
        """
      val metaData ="Joe Bloggs"
      validate(metaData, schema) must beLike { case Success(_) => ok }
    }

    "succeed for nested if where condition is true" in {
      val schema =
        """version 1.0
           @totalColumns 1 @noHeader
           col1: if(starts("Joe"), if( starts("Bloggs"), length(4), length(1, 20) ) )
        """
      val metaData ="Joe Bloggs"
      validate(metaData, schema) must beLike { case Success(_) => ok }
    }

    "fail for nested if where condition is true" in {
      val schema =
        """version 1.0
           @totalColumns 1 @noHeader
           col1: if(starts("Joe"), if( starts("Joe"), length(4), length(1, 20) ) )
        """
      val metaData ="Joe Bloggs"
      validate(metaData, schema) must beLike {
        case Failure(messages) => messages.list mustEqual List(ErrorMessage("""length(4) fails for line: 1, column: col1, value: "Joe Bloggs""""))
      }
    }

    "succeed for nested if where condition is false" in {
      val schema =
        """version 1.0
           @totalColumns 1 @noHeader
           col1: if(starts("Bloggs"), if( starts("Joe"), length(4), length(1, 20) ) , if( starts("Joe"), length(4, 20), length(4) ) )
        """
      val metaData ="Joe Bloggs"
      validate(metaData, schema) must beLike { case Success(_) => ok }
    }


    "succeed when 'and'ing rules together" in {
      val schema =
        """version 1.0
           @totalColumns 1 @noHeader
           col1: starts("Dath") and ends("Vader")
        """
      val metaData ="Dath Vader"
      validate(metaData, schema) must beLike { case Success(_) => ok }
    }

    "succeed with parentheses creating a match, same as below with () difference" in {
      val schema =
        """version 1.0
           @totalColumns 1 @noHeader
           col1: is("True") or (is("True") and is("False"))
        """
      val metaData ="True"
      validate(metaData, schema) must beLike { case Success(_) => ok }
    }

    "fail with parentheses creating a failed match, same example as above, only () difference" in {
      val schema =
        """version 1.0
           @totalColumns 1 @noHeader
           col1: (is("True") or is("True")) and is("False")
        """
      val metaData ="True"
      validate(metaData, schema) must beLike {
        case Failure(messages) => messages.list mustEqual List(ErrorMessage("""(is("True") or is("True")) and is("False") fails for line: 1, column: col1, value: "True""""))
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

      validate(metaData, schema) must beLike { case Success(_) => ok }
    }

  }
}