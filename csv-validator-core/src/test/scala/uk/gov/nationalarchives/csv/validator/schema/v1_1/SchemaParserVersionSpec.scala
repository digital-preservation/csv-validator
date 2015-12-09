/**
 * Copyright (c) 2013, The National Archives <digitalpreservation@nationalarchives.gov.uk>
 * http://www.nationalarchives.gov.uk
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package uk.gov.nationalarchives.csv.validator.schema.v1_1

import java.io.StringReader

import uk.gov.nationalarchives.csv.validator.TestResources
import uk.gov.nationalarchives.csv.validator.schema._

class SchemaParserVersionSpec extends SchemaSpecBase with TestResources{

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

      parse(new StringReader(schema)) must beLike { case Success(parsedSchema, _) => parsedSchema mustEqual buildSchema1_0(TotalColumns(3),NoHeader())(columnDefinitions:_*) }
    }

    "fail if the schema version is wrong" in {
      val schema = """version 1
                      @totalColumns 1
                      LastName: @IgnoreCase regex ("[a]")"""

      parse(new StringReader(schema)) must beLike {
        case Failure(messages, _) => messages mustEqual s"Schema version declaration 'version 1.0' missing or incorrect"
      }
    }

    "fail if the schema version is not supported" in {
      val schema = """version 2.0
                      @totalColumns 1
                      LastName: @IgnoreCase regex ("[a]")"""

      parse(new StringReader(schema)) must beLike {
        case Failure(messages, _) => messages mustEqual s"Invalid schema version. This version of the csv validator supports only 1.1"
      }
    }

    "fail if the schema defined rule that are only supported by future version - upperCase" in {
      def schema(version: String) = s"""version $version
                      @totalColumns 1
                      LastName: upperCase"""

      parse(new StringReader(schema("1.1"))).successful mustEqual true

      parse(new StringReader(schema("1.0"))) must beLike {
        case Failure(messages, _) => messages mustEqual s"Invalid column definition"
      }
    }

    "fail if the schema defined rule that are only supported by future version - unbound range" in {
      def schema(version: String) = s"""version $version
                      @totalColumns 1
                      Age: range(1,*)"""

      parse(new StringReader(schema("1.1"))).successful mustEqual true

      parse(new StringReader(schema("1.0"))).successful mustEqual false
    }

    "fail if the schema defined rule that are only supported by future version - any" in {
      def schema(version: String) = s"""version $version
                      @totalColumns 1
                      LastName: any("a","b","c")"""

      parse(new StringReader(schema("1.1"))).successful mustEqual true

      parse(new StringReader(schema("1.0"))) must beLike {
        case Failure(messages, _) => messages mustEqual s"Invalid column definition"
      }
    }


    "fail if the schema defined rule that are only supported by future version - integrityCheck" in {
      def schema(version: String) = s"""version $version
                      @totalColumns 1
                      LastName: integrityCheck("excludeFolder")"""

      parse(new StringReader(schema("1.1"))).successful mustEqual true

      parse(new StringReader(schema("1.0"))) must beLike {
        case Failure(messages, _) => messages mustEqual s"Invalid column definition"
      }
    }

    "fail if the schema defined rule that are only supported by future version - identical" in {
      def schema(version: String) = s"""version $version
                      @totalColumns 1
                      LastName: identical"""

      parse(new StringReader(schema("1.1"))).successful mustEqual true

      parse(new StringReader(schema("1.0"))) must beLike {
        case Failure(messages, _) => messages mustEqual s"Invalid column definition"
      }
    }


    "fail if the schema defined rule that are only supported by future version - switch" in {
      def schema(version: String) = s"""version $version
                      @totalColumns 1
                      LastName: identical"""

      parse(new StringReader(schema("1.1"))).successful mustEqual true

      parse(new StringReader(schema("1.0"))) must beLike {
        case Failure(messages, _) => messages mustEqual s"Invalid column definition"
      }
    }

    "fail if the schema defined rule that are only supported by future version - switch" in {
      val Name = "$Name"
      def schema(version: String) = s"""version $version
                                        |    @totalColumns 2
                                        |    Name:
                                        |    SomeSwitchRule: switch(($Name/starts("hello"),is("hello world")),($Name/starts("HELLO"),is("HELLO WORLD")))""".stripMargin

      parse(new StringReader(schema("1.1"))).successful mustEqual true

      parse(new StringReader(schema("1.0"))) must beLike {
        case Failure(messages, _) => messages mustEqual s"Invalid column definition"
      }
    }

  }
}