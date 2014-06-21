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
import scalaz.{Failure => FailureZ}
import uk.gov.nationalarchives.csv.validator.SchemaMessage


class SchemaParserColumnDirectivesSpec extends Specification {

  object TestSchemaParser extends SchemaParser { val pathSubstitutions = List[(String,String)](); val enforceCaseSensitivePathChecks = false }

  import TestSchemaParser._

  "Schema" should {

    "succeed for a @optional column directive" in {
      val schema = """version 1.0
                      |@totalColumns 1
                      |column1: @optional""".stripMargin

      parse(new StringReader(schema)) must beLike { case Success(Schema(_, List(ColumnDefinition("column1", _, List(Optional())))),_) => ok }
    }

    "succeed for a @ignoreCase column directive" in {
      val schema = """version 1.0
                      @totalColumns 1
                      column1: @ignoreCase""".stripMargin

      parse(new StringReader(schema)) must beLike { case Success(Schema(_, List(ColumnDefinition("column1", _, List(IgnoreCase())))),_) => ok }
    }

    "fail for duplicate column directives" in {
      val schema = """version 1.0
                     |@totalColumns 1
                     |column1: @ignoreCase @ignoreCase""".stripMargin

      parseAndValidate(new StringReader(schema)) must beLike { case FailureZ(msgs) => msgs.list mustEqual List(SchemaMessage(
        """[3.23] failure: `warning' expected but `i' found
        |
        |column1: @ignoreCase @ignoreCase
        |                      ^""".stripMargin)) }
    }

    "fail for multiple duplicate column directives" in {
      val schema = """version 1.0
                     |@totalColumns 1
                     |column1: @ignoreCase @optional @ignoreCase @optional""".stripMargin

      parseAndValidate(new StringReader(schema)) must beLike { case FailureZ(msgs) => msgs.list mustEqual List(SchemaMessage(
       """[3.33] failure: `warning' expected but `i' found
       |
       |column1: @ignoreCase @optional @ignoreCase @optional
       |                                ^""".stripMargin)) }
    }

    "fail for duplicate column directives on different columns" in {
      val schema = """version 1.0
                     |@totalColumns 3
                     |column1: @ignoreCase @optional @ignoreCase @optional
                     |column2: @optional @ignoreCase
                     |column3: @ignoreCase @ignoreCase @optional @optional""".stripMargin

      parseAndValidate(new StringReader(schema)) must beLike { case FailureZ(msgs) => msgs.list mustEqual List(SchemaMessage(
        """[3.33] failure: `warning' expected but `i' found
          |
          |column1: @ignoreCase @optional @ignoreCase @optional
          |
          |                                ^""".stripMargin)) }
    }

  }

  "Schema ordering" should {
    "allow any ordering of column directives - optional before ignore case" in {
      val schema = """version 1.0
                      |@totalColumns 1
                      |column1: @optional @ignoreCase""".stripMargin

      parse(new StringReader(schema)) must beLike { case Success(Schema(_, List(ColumnDefinition("column1", _, Optional() :: IgnoreCase() :: Nil))),_) => ok }
    }

    "allow any ordering of column directives - ignore case before optional" in {
      val schema = """version 1.0
                      @totalColumns 1
                      column1: @ignoreCase @optional""".stripMargin

      parse(new StringReader(schema)) must beLike { case Success(Schema(_, List(ColumnDefinition("column1", _, IgnoreCase() :: Optional() :: Nil))),_) => ok }
    }
  }
}