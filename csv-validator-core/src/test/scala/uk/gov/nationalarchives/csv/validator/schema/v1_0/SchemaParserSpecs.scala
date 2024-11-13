/*
 * Copyright (c) 2013, The National Archives <digitalpreservation@nationalarchives.gov.uk>
 * https://www.nationalarchives.gov.uk
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package uk.gov.nationalarchives.csv.validator.schema.v1_0

import java.io.StringReader
import uk.gov.nationalarchives.csv.validator.schema._

class SchemaParserSpecs extends SchemaSpecBase {

  import TestSchemaParser._

  "Schema" should {

    "succeed for valid minimal schema" in {
      val columnDefinitions = List(new ColumnDefinition(NamedColumnIdentifier("column1")),new ColumnDefinition(NamedColumnIdentifier("column2")),new ColumnDefinition(NamedColumnIdentifier("column3")))

      val schema = """version 1.0
                     |@totalColumns 3
                     |@noHeader
                     |column1:
                     |column2:
                     |column3:""".stripMargin

      parse(new StringReader(schema)) must beLike { case Success(parsedSchema, _) => parsedSchema mustEqual buildSchema1_0(TotalColumns(3), NoHeader())(columnDefinitions:_*) }
    }

    "fail if the schema version is wrong" in {
      val schema = """version 1
                      @totalColumns 1
                      LastName: @IgnoreCase regex ("[a]")"""

      parse(new StringReader(schema)) must beLike {
        case Failure(messages, _) => messages mustEqual "Error parsing schema version 1"
      }
    }


    "succeed for extra white space around (including tabs)" in {
      val schema = """version 1.0
                      @totalColumns 2
                      Name :
                      Age   :     """

      parse(new StringReader(schema)) must beLike { case Success(parsedSchema, _) => parsedSchema mustEqual buildSchema1_0(TotalColumns(2))(namedColumn("Name"), namedColumn("Age")) }
    }

    "fail if column directives declared before rules" in {
      val schema = """version 1.0
                      @totalColumns 1
                      LastName: @IgnoreCase regex ("[a]")"""

      parse(new StringReader(schema)) must beLike {
        case Failure(messages, _) => messages mustEqual "Invalid column definition"
      }
    }

    "succeed if noHeader global directive set" in {
      val schema = """version 1.0
                      @totalColumns 2 @noHeader
                      Name :
                      Age   :     """
      parse(new StringReader(schema)) must beLike { case Success(parsedSchema, _) => parsedSchema mustEqual buildSchema1_0(TotalColumns(2), NoHeader())(namedColumn("Name"), namedColumn("Age")) }
    }

    val nonEmptySchema = buildSchema1_0(TotalColumns(2))(nonEmptyColumn("Name"), nonEmptyColumn("Age"))
    "succeed for single-line comments" should {

      "placed immediately after the prolog" in {
        val schema = """version 1.0
          @totalColumns 2
          //start of body
          Name : notEmpty
          Age : notEmpty
          """
        parse(new StringReader(schema)) must beLike { case Success(parsedSchema, _) => parsedSchema mustEqual nonEmptySchema }
      }

      "placed anywhere in the body" in {
        val schema = """version 1.0
          @totalColumns 2
          //start of body
          Name : notEmpty //inline comment
          //comment before next rule
          Age : notEmpty //another inline comment
                     """
       parse(new StringReader(schema)) must beLike { case Success(parsedSchema, _) => parsedSchema mustEqual nonEmptySchema }
      }

      "placed at the end of the body (with newline)" in {
        val schema = """version 1.0
          @totalColumns 2
          Name : notEmpty
          Age : notEmpty
          //comment at end of body
                     """
        parse(new StringReader(schema)) must beLike { case Success(parsedSchema, _) => parsedSchema mustEqual nonEmptySchema }
      }

      "placed at the end of the body (without newline)" in {
        val schema = """version 1.0
          @totalColumns 2
          Name : notEmpty
          Age : notEmpty
          //comment at end of body"""
        parse(new StringReader(schema)) must beLike { case Success(parsedSchema, _) => parsedSchema mustEqual nonEmptySchema  }
      }

    }

    "succeed for multi-line comments" should {

      "placed immediately after the prolog" in {
        val schema = """version 1.0
          @totalColumns 2
          /*
            start of body
          */
          Name : notEmpty
          Age : notEmpty
                     """
        parse(new StringReader(schema)) must beLike { case Success(parsedSchema, _) => parsedSchema mustEqual nonEmptySchema  }
      }

      "placed anywhere in the body" in {
        val schema = """version 1.0
          @totalColumns 2
          /*
            start of body
          */
          Name : notEmpty /* inline
          comment */
          /* comment before next rule */
          Age : notEmpty /* another inline
          comment */
                     """
        parse(new StringReader(schema)) must beLike { case Success(parsedSchema, _) => parsedSchema mustEqual nonEmptySchema }
      }

      "placed at the end of the body (with newline)" in {
        val schema = """version 1.0
          @totalColumns 2
          Name : notEmpty
          Age : notEmpty
          /* comment at
            end of body */
                     """
        parse(new StringReader(schema)) must beLike { case Success(parsedSchema, _) => parsedSchema mustEqual nonEmptySchema }
      }

      "placed at the end of the body (without newline)" in {
        val schema = """version 1.0
          @totalColumns 2
          Name : notEmpty
          Age : notEmpty
          /* comment at end
          of body */"""
        parse(new StringReader(schema)) must beLike { case Success(parsedSchema, _) => parsedSchema mustEqual nonEmptySchema }
      }

    }

  }




  "When there is a named column for a rule the schema" should {
    "succeed when there is a named column for a rule" in {

      val columnDefinitions = List(new ColumnDefinition(NamedColumnIdentifier("FirstName"), Nil, Nil),new ColumnDefinition(NamedColumnIdentifier("LastName"), List( IsRule(Literal(Some("Yoda")))))   )

      val schema = """version 1.0
                      @totalColumns 2  @noHeader
                      FirstName:
                      LastName: $FirstName/is("Yoda")"""

      parse(new StringReader(schema)) must beLike { case Success(parsedSchema, _) => parsedSchema mustEqual buildSchema1_0(TotalColumns(2), NoHeader())(columnDefinitions:_*) }
    }

    "succeed when there is NO named column for a rule" in {

      val columnDefinitions = List(new ColumnDefinition(NamedColumnIdentifier("FirstName"), Nil, Nil),new ColumnDefinition(NamedColumnIdentifier("LastName"), List( IsRule(Literal(Some("Yoda")))))   )

      val schema = """version 1.0
                      @totalColumns 2  @noHeader
                      FirstName:
                      LastName: is("Yoda")"""

      parse(new StringReader(schema)) must beLike { case Success(parsedSchema, _) => parsedSchema mustEqual buildSchema1_0(TotalColumns(2), NoHeader())(columnDefinitions:_*) }
    }

    "succeed when there is 'or' rule named column for a rule" in {

      val columnDefinitions = List(new ColumnDefinition(NamedColumnIdentifier("FirstName"), Nil, Nil),new ColumnDefinition(NamedColumnIdentifier("LastName"), List( OrRule( IsRule(Literal(Some("Yoda"))),IsRule(Literal(Some("Darth"))) ) )) )

      val schema = """version 1.0
                      @totalColumns 2  @noHeader
                      FirstName:
                      LastName: $FirstName/is("Yoda") or $FirstName/is("Darth")"""

      parse(new StringReader(schema)) must beLike { case Success(parsedSchema, _) => parsedSchema mustEqual buildSchema1_0(TotalColumns(2), NoHeader())(columnDefinitions:_*) }
    }


    "fail if is an invalid explisit column format" in {
      val schema = """version 1.0
                      @totalColumns 2  @noHeader
                      FirstName:
                      LastName: WRONGCOLUMN/is("Yoda") or $FirstName/is("Darth")"""

      parse(new StringReader(schema)) must beLike {
        case Failure(messages, _) => messages mustEqual "Invalid column definition"
      }
    }

  }
}
