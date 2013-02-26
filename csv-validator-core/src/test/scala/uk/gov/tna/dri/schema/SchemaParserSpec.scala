package uk.gov.tna.dri.schema

import org.specs2.mutable._
import org.specs2.matcher.ParserMatchers
import java.io.StringReader

class SchemaParserSpec extends Specification with ParserMatchers {

  object TestSchemaParser extends SchemaParser

  override val parsers = TestSchemaParser

  import TestSchemaParser._

  "Schema" should {
    val globalDirsTwo = List(TotalColumns(2))
    val globalDirsThree = List(TotalColumns(3))

    "succeed for valid minimal schema" in {
      val columnDefinitions = List(new ColumnDefinition("column1"),new ColumnDefinition("column2"),new ColumnDefinition("column3"))

      val schema = """version 1.0
                      @totalColumns 3
                      column1:
                      column2:
                      column3:"""

      parse(new StringReader(schema)) must beLike { case Success(schema, _) => schema mustEqual Schema(globalDirsThree, columnDefinitions) }
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

      parse(new StringReader(schema)) must beLike { case Success(schema, _) => schema mustEqual Schema(globalDirsTwo, List(ColumnDefinition("Name"), ColumnDefinition("Age"))) }
    }

    "fail if column directives declared before rules" in {
      val schema = """version 1.0
                      @totalColumns 1
                      LastName: @IgnoreCase regex ("[a]")"""

      parse(new StringReader(schema)) must beLike {
        case Failure(messages, _) => messages mustEqual "Column definition contains invalid text"
      }
    }

    "succeed if noHeader global directive set :" in {
      val schema = """version 1.0
                      @totalColumns 2 @noHeader
                      Name :
                      Age   :     """
      val globalDirsTwoHeaderSet = List(TotalColumns(2), NoHeader())
      parse(new StringReader(schema)) must beLike { case Success(schema, _) => schema mustEqual Schema(globalDirsTwoHeaderSet, List(ColumnDefinition("Name"), ColumnDefinition("Age"))) }
    }
  }
}