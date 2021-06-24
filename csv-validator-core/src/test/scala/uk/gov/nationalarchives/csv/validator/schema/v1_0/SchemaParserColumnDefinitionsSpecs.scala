/**
 * Copyright (c) 2013, The National Archives <digitalpreservation@nationalarchives.gov.uk>
 * https://www.nationalarchives.gov.uk
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package uk.gov.nationalarchives.csv.validator.schema.v1_0

import java.io.StringReader


import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import uk.gov.nationalarchives.csv.validator.{SchemaDefinitionError, FailMessage}
import uk.gov.nationalarchives.csv.validator.schema._

import scalaz.{Failure => FailureZ, Success => SuccessZ, IList}

@RunWith(classOf[JUnitRunner])
class SchemaParserColumnDefinitionsSpecs extends SchemaSpecBase {

  import TestSchemaParser._

  "Schema" should {
    val globalDirsOne = List(TotalColumns(1))
    val globalDirsTwo = List(TotalColumns(2))

    "succeed for valid schema with all possible column definitions" in {
      val columnDefinitions = List(new ColumnDefinition(NamedColumnIdentifier("column1")),new ColumnDefinition(NamedColumnIdentifier("column2")),new ColumnDefinition(NamedColumnIdentifier("column3")),
        new ColumnDefinition(NamedColumnIdentifier(".")),new ColumnDefinition(NamedColumnIdentifier("_-co.l")),new ColumnDefinition(NamedColumnIdentifier("0.a-B-z_Z")),new ColumnDefinition(NamedColumnIdentifier("-abc.csvs")))

      val schema = """version 1.0
                      @totalColumns 7
                      column1:
                      column2:
                      column3:
                      .:
                      _-co.l:
                      0.a-B-z_Z:
                      -abc.csvs:"""

      parse(new StringReader(schema)) must beLike { case Success(schemaResult, _) => schemaResult mustEqual buildSchema1_0(TotalColumns(7))(columnDefinitions:_*) }
    }

    "succeed for valid schema with all possible quoted column definitions" in {
      val columnDefinitions = List(new ColumnDefinition(NamedColumnIdentifier("column1")),new ColumnDefinition(NamedColumnIdentifier("column2")),new ColumnDefinition(NamedColumnIdentifier("column 3")),
        new ColumnDefinition(NamedColumnIdentifier("column 4/5")), new ColumnDefinition(NamedColumnIdentifier(".")),new ColumnDefinition(NamedColumnIdentifier("_-co.l")),
        new ColumnDefinition(NamedColumnIdentifier("0.a-B-z_Z")),new ColumnDefinition(NamedColumnIdentifier("-abc.csvs")))

      val schema = """version 1.0
                      @totalColumns 8
                      "column1":
                      "column2":
                      "column 3":
                      "column 4/5":
                      ".":
                      "_-co.l":
                      "0.a-B-z_Z":
                      "-abc.csvs":"""

      parse(new StringReader(schema)) must beLike { case Success(schemaResult, _) => schemaResult mustEqual buildSchema1_0(TotalColumns(8))(columnDefinitions:_*) }
    }

    "fail if column ident contains an invalid char ie not 0-9 a-z A-Z . - _" in {
      val schema = """version 1.0
      @totalColumns 1
      column1':"""
      parse(new StringReader(schema)) must beLike {
        case Failure(messages, _) => messages mustEqual "Invalid column definition"
      }
    }

    "fail if quoted column ident contains a quote" in {
      val schema = """version 1.0
      @totalColumns 2
      "column "1":
      "column "2":"""
      parse(new StringReader(schema)) must beLike {
        case Failure(messages, _) => messages mustEqual "Invalid column definition"
      }
    }

    "fail if the total number of columns does not match the number of column definitions" in {
      val schema = """version 1.0
                      |@totalColumns 2
                      |LastName: regex ("[a]")""".stripMargin

      parseAndValidate(new StringReader(schema)) must beLike { case FailureZ(msgs) => msgs.list mustEqual IList(FailMessage(SchemaDefinitionError, "@totalColumns = 2 but number of columns defined = 1 at line: 2, column: 1")) }
    }

    "fail for invalid column identifier" in {
      val schema = """version 1.0
                      @totalColumns 1
                      Last Name """

      parse(new StringReader(schema)) must beLike { case Failure(message, _) => message mustEqual "Invalid column definition" }
    }

    "succeed for column definition with no rules" in {
      val schema = """version 1.0
                      @totalColumns 1
                      Name:"""

      parse(new StringReader(schema)) must beLike { case Success(schemaResult, _) => schemaResult mustEqual buildSchema1_0(globalDirsOne:_*)(namedColumn("Name")) }
    }

    "succeed for column definition with single regex rule" in {
      val schema = """version 1.0
                      @totalColumns 1
                      Age: regex ("[1-9]*")"""

      parse(new StringReader(schema)) must beLike { case Success(Schema(globalDirsOne1, List(ColumnDefinition(NamedColumnIdentifier("Age"), List(RegExpRule(r)), _)), v), _) => r mustEqual "[1-9]*" }
    }

    "fail for more than one column definition on a line" in {
      val schema = """version 1.0
                      @totalColumns 1
                      LastName: regex ("[a-z]*") Age"""

      parse(new StringReader(schema)) must beLike { case Failure(message, _) => message mustEqual """Invalid column definition""" }
    }

    "fail for extra text after column definition on a line" in {
      val schema = """version 1.0
                      @totalColumns 3
                      LastName: regex ("[a-z]*")
                      FirstName: dfsdfsdfwe
                      Age:"""

      parse(new StringReader(schema)) must beLike { case Failure(message, _) => message mustEqual "Invalid column definition" }
    }

    "fail when one invalid column reference" in {
      val schema = """version 1.0
                     @totalColumns 2
                    |Column1: in($NotAColumn)
                    |Column2:""".stripMargin

      parseAndValidate(new StringReader(schema)) must beLike {
        case FailureZ(msgs) => msgs.list mustEqual IList(FailMessage(SchemaDefinitionError, "Column: Column1 has invalid cross reference in($NotAColumn) at line: 3, column: 10"))
      }
    }

    "fail when there are two rules and one is invalid" in {
      val schema = """version 1.0
                     @totalColumns 2
                    |Column1: in($Column2) in($NotAColumn2)
                    |Column2:""".stripMargin

      parseAndValidate(new StringReader(schema)) must beLike {
        case FailureZ(msgs) => msgs.list mustEqual IList(FailMessage(SchemaDefinitionError, "Column: Column1 has invalid cross reference in($NotAColumn2) at line: 3, column: 23"))
      }
    }

    "fail when two columns have two rules and each has one invalid column" in {
      val schema ="""version 1.0
                     @totalColumns 2
                    |Column1: in($Column2) in($NotAColumn2)
                    |Column2: in($NotAColumn3) in($Column2)""".stripMargin

      parseAndValidate(new StringReader(schema)) must beLike {
        case FailureZ(msgs) => msgs.list mustEqual IList(FailMessage(SchemaDefinitionError,
          """Column: Column1 has invalid cross reference in($NotAColumn2) at line: 3, column: 23
            |Column: Column2 has invalid cross reference in($NotAColumn3) at line: 4, column: 10""".stripMargin))
      }
    }

    "fail when two columns have two rules and each has one invalid column with different rules" in {
      val schema ="""version 1.0
                    |@totalColumns 2
                    |Column1: is($Column1) is($NotAColumn1)
                    |Column2: not($Column2) not($NotAColumn2)
                    |Column3: in($Column3) in($NotAColumn3)
                    |Column4: starts($Column4) starts($NotAColumn4)
                    |Column5: ends($Column5) ends($NotAColumn5)""".stripMargin

      parseAndValidate(new StringReader(schema)) must beLike {
        case FailureZ(msgs) => msgs.list mustEqual IList(FailMessage(SchemaDefinitionError, """@totalColumns = 2 but number of columns defined = 5 at line: 2, column: 1
                                                        |Column: Column1 has invalid cross reference is($NotAColumn1) at line: 3, column: 23
                                                        |Column: Column2 has invalid cross reference not($NotAColumn2) at line: 4, column: 24
                                                        |Column: Column3 has invalid cross reference in($NotAColumn3) at line: 5, column: 23
                                                        |Column: Column4 has invalid cross reference starts($NotAColumn4) at line: 6, column: 27
                                                        |Column: Column5 has invalid cross reference ends($NotAColumn5) at line: 7, column: 25""".stripMargin))
      }
    }

    "fail for multiple columns with same name" in {
      val schema = """version 1.0
                      @totalColumns 4
                      Column1:
                      Column2:
                      Column1: regex("A")
                      Column2:"""

      parseAndValidate(new StringReader(schema)) must beLike {
        case FailureZ(msgs) => msgs.list mustEqual IList(FailMessage(SchemaDefinitionError, """Column: Column1 has duplicates on lines 3, 5
                                                          |Column: Column2 has duplicates on lines 4, 6""".stripMargin))
      }
    }

    "succeed if Column1 correctly has InRule that points to Column2" in {
      val schema = """version  1.0
                      @totalColumns 2
                      Column1: in($Column2)
                      Column2:"""

      parseAndValidate(new StringReader(schema)) must beLike {
        case SuccessZ(schemaResult) => schemaResult mustEqual buildSchema1_0(globalDirsTwo:_*)(ColumnDefinition(NamedColumnIdentifier("Column1"), List(InRule(ColumnReference(NamedColumnIdentifier("Column2"))))),
                                                                               namedColumn("Column2"))
      }
    }
  }
}