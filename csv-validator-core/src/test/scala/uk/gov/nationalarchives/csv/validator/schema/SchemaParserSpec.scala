/**
 * Copyright (c) 2013, The National Archives <digitalpreservation@nationalarchives.gov.uk>
 * http://www.nationalarchives.gov.uk
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package uk.gov.nationalarchives.csv.validator.schema

import org.specs2.mutable._
import java.io.StringReader

class SchemaParserSpec extends Specification {

  object TestSchemaParser extends SchemaParser { val pathSubstitutions = List[(String,String)](); val enforceCaseSensitivePathChecks = false }

  import TestSchemaParser._

  "Schema" should {

    "succeed for valid minimal schema" in {
      val columnDefinitions = List(new ColumnDefinition("column1"),new ColumnDefinition("column2"),new ColumnDefinition("column3"))

      val schema = """version 1.0
                     |@totalColumns 3
                     |@noHeader
                     |column1:
                     |column2:
                     |column3:""".stripMargin

      parse(new StringReader(schema)) must beLike { case Success(parsedSchema, _) => parsedSchema mustEqual Schema(List(TotalColumns(3),NoHeader()), columnDefinitions) }
    }

    "fail if the schema version is wrong" in {
      val schema = """version 1
                      @totalColumns 1
                      LastName: @IgnoreCase regex ("[a]")"""

      parse(new StringReader(schema)) must beLike {
        case Failure(messages, _) => messages mustEqual s"version 1.0 missing or incorrect"
      }
    }

    "succeed for extra white space around (including tabs) :" in {
      val schema = """version 1.0
                      @totalColumns 2
                      Name :
                      Age   :     """

      parse(new StringReader(schema)) must beLike { case Success(parsedSchema, _) => parsedSchema mustEqual Schema(List(TotalColumns(2)), List(ColumnDefinition("Name"), ColumnDefinition("Age"))) }
    }

    "fail if column directives declared before rules" in {
      val schema = """version 1.0
                      @totalColumns 1
                      LastName: @IgnoreCase regex ("[a]")"""

      parse(new StringReader(schema)) must beLike {
        case Failure(messages, _) => messages mustEqual "Invalid schema text"
      }
    }

    "succeed if noHeader global directive set :" in {
      val schema = """version 1.0
                      @totalColumns 2 @noHeader
                      Name :
                      Age   :     """
      parse(new StringReader(schema)) must beLike { case Success(parsedSchema, _) => parsedSchema mustEqual Schema(List(TotalColumns(2), NoHeader()), List(ColumnDefinition("Name"), ColumnDefinition("Age"))) }
    }
  }


  "When there is a named column for a rule the schema" should {
    "succeed when there is a named column for a rule" in {

      val columnDefinitions = List(new ColumnDefinition("FirstName", Nil, Nil),new ColumnDefinition("LastName", List( IsRule(Literal(Some("Yoda")))))   )

      val schema = """version 1.0
                      @totalColumns 2  @noHeader
                      FirstName:
                      LastName: $FirstName/is("Yoda")"""

      parse(new StringReader(schema)) must beLike { case Success(parsedSchema, _) => parsedSchema mustEqual Schema(List(TotalColumns(2), NoHeader()), columnDefinitions) }
    }

    "succeed when there is NO named column for a rule" in {

      val columnDefinitions = List(new ColumnDefinition("FirstName", Nil, Nil),new ColumnDefinition("LastName", List( IsRule(Literal(Some("Yoda")))))   )

      val schema = """version 1.0
                      @totalColumns 2  @noHeader
                      FirstName:
                      LastName: is("Yoda")"""

      parse(new StringReader(schema)) must beLike { case Success(parsedSchema, _) => parsedSchema mustEqual Schema(List(TotalColumns(2), NoHeader()), columnDefinitions) }
    }

    "succeed when there is 'or' rule named column for a rule" in {

      val columnDefinitions = List(new ColumnDefinition("FirstName", Nil, Nil),new ColumnDefinition("LastName", List( OrRule( IsRule(Literal(Some("Yoda"))),IsRule(Literal(Some("Darth"))) ) )) )

      val schema = """version 1.0
                      @totalColumns 2  @noHeader
                      FirstName:
                      LastName: $FirstName/is("Yoda") or $FirstName/is("Darth")"""

      parse(new StringReader(schema)) must beLike { case Success(parsedSchema, _) => parsedSchema mustEqual Schema(List(TotalColumns(2), NoHeader()), columnDefinitions) }
    }


    "fail if is an invalid explisit column format" in {
      val schema = """version 1.0
                      @totalColumns 2  @noHeader
                      FirstName:
                      LastName: WRONGCOLUMN/is("Yoda") or $FirstName/is("Darth")"""

      parse(new StringReader(schema)) must beLike {
        case Failure(messages, _) => messages mustEqual "Invalid schema text"
      }
    }

  }
}