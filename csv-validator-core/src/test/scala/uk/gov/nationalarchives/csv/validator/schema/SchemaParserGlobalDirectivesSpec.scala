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
import scalaz.{Failure => FailureZ}
import java.io.StringReader


class SchemaParserGlobalDirectivesSpec extends Specification {

  object TestSchemaParser extends SchemaParser { val pathSubstitutions = List[(String,String)](); val enforceCaseSensitivePathChecks = false }

  import TestSchemaParser._

  "Schema" should {

    "succeed for a @totalColumns global directive" in {
      val schema =
        """version 1.0
           |@totalColumns 1
           |column1: """.stripMargin

      parse(new StringReader(schema)) must beLike { case Success(Schema(List(TotalColumns(_)), List(ColumnDefinition("column1", Nil, Nil))),_) => ok }
    }

    "with @totalColumns and @noHeader global directives" should {
      "succeed on seperate lines" in {
        val schema =
          """version 1.0
            |@totalColumns 1
            |@noHeader
            |column1: """.stripMargin

        parse(new StringReader(schema)) must beLike { case Success(Schema(List(TotalColumns(_), NoHeader()), List(ColumnDefinition("column1", Nil, Nil))), _) => ok}
      }

      "succeed on same line" in {
        val schema =
          """version 1.0
            |@totalColumns 1 @noHeader
            |column1: """.stripMargin

        parse(new StringReader(schema)) must beLike { case Success(Schema(List(TotalColumns(_), NoHeader()), List(ColumnDefinition("column1", Nil, Nil))), _) => ok}
      }
    }

    "@noHeader and @ignoreColumnNameCase global directives (mutually exclusive)" should {
      "fail for @noHeader followed by @ignoreColumnNameCase" >> pending("Need to improve error messages") {
        val schema =
          """version 1.0
            |@noHeader
            |@ignoreColumnNameCase
            |column1: """.stripMargin

        parse(new StringReader(schema)) must beLike { case Failure(message, _) => message mustEqual "Invalid global directive" }
      }

      "fail for @ignoreColumnNameCase followed by @noHeader" >> pending("Need to improve error messages") {
        val schema =
          """version 1.0
            |@ignoreColumnNameCase
            |@noHeader
            |column1: """.stripMargin

        parse(new StringReader(schema)) must beLike { case Failure(message, _) => message mustEqual "Invalid global directive" }
      }
    }

    "succeed with no global directives" in {
      val schema =
        """version 1.0
          |column1: """.stripMargin

      parse(new StringReader(schema)) must beLike { case Success(Schema(Nil, List(ColumnDefinition("column1", Nil, Nil))), _) => ok}
    }

    "succeed with @permitEmpty" in {
      val schema =
        """version 1.0
          |@permitEmpty
          |column1: """.stripMargin

      parse(new StringReader(schema)) must beLike { case Success(Schema(List(PermitEmpty()), List(ColumnDefinition("column1", Nil, Nil))), _) => ok}
    }

    "succeed with @permitEmpty and @noHeader" in {
      val schema =
        """version 1.0
          |@permitEmpty
          |@noHeader
          |column1: """.stripMargin

      parse(new StringReader(schema)) must beLike { case Success(Schema(List(PermitEmpty(), NoHeader()), List(ColumnDefinition("column1", Nil, Nil))), _) => ok}
    }

  }
}