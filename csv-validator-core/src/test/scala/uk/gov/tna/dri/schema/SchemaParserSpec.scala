package uk.gov.tna.dri.schema

import org.specs2.mutable._
import java.io.StringReader

class SchemaParserSpec extends Specification {

  object TestSchemaParser extends SchemaParser { val pathSubstitutions = List[(String,String)]() }

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
}