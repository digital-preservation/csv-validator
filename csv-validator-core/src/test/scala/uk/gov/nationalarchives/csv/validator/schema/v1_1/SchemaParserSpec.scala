/*
 * Copyright (c) 2013, The National Archives <digitalpreservation@nationalarchives.gov.uk>
 * https://www.nationalarchives.gov.uk
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package uk.gov.nationalarchives.csv.validator.schema.v1_1

import java.io.StringReader

import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import uk.gov.nationalarchives.csv.validator.schema._
import uk.gov.nationalarchives.csv.validator.schema.v1_0.IsRule

@RunWith(classOf[JUnitRunner])
class SchemaParserSpec extends SchemaSpecBase {

  import TestSchemaParser._

  "Schema" should {

    "succeed for valid minimal schema" in {
      val columnDefinitions = List(new ColumnDefinition(NamedColumnIdentifier("column1")), new ColumnDefinition(NamedColumnIdentifier("column2"),List(IsRule(NoExt(ColumnReference(NamedColumnIdentifier("column1"))))),List()), new ColumnDefinition(NamedColumnIdentifier("column3")))

      val schema =
        """version 1.1
                     |@totalColumns 3
                     |@noHeader
                     |column1:
                     |column2: is(noExt($column1))
                     |column3:""".stripMargin

      parse(new StringReader(schema)) must beLike { case Success(parsedSchema, _) => parsedSchema mustEqual buildSchema1_1(TotalColumns(3), NoHeader())(columnDefinitions:_*) }
    }

    "succeed for valid minimal schema" in {
      val columnDefinitions = List(new ColumnDefinition(NamedColumnIdentifier("c1")), new ColumnDefinition(NamedColumnIdentifier("c2")),new ColumnDefinition(NamedColumnIdentifier("c3"),List(IsRule(Concat(ColumnReference(NamedColumnIdentifier("c1")), ColumnReference(NamedColumnIdentifier("c2"))))),List()))

      val schema =
        """version 1.1
          |@totalColumns 3
          |c1:
          |c2:
          |c3: is(concat($c1,$c2))""".stripMargin

      parse(new StringReader(schema)) must beLike { case Success(parsedSchema, _) => parsedSchema mustEqual buildSchema1_1(TotalColumns(3))(columnDefinitions:_*) }
    }
  }
}
