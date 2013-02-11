package uk.gov.tna.dri.schema

import org.specs2.mutable._
import org.specs2.matcher.ParserMatchers
import java.io.StringReader

class SchemaParserColumnDirectivesSpec extends Specification with ParserMatchers {

  object TestSchemaParser extends SchemaParser

  override val parsers = TestSchemaParser

  import TestSchemaParser._

  "Schema" should {

    "succeed for a @Optional column directive" in {
      val schema = """@TotalColumns 1
                      column1: @Optional"""

      parse(new StringReader(schema)) must beLike { case Success(Schema(_, List(ColumnDefinition("column1", _, List(Optional())))),_) => ok }
    }

    "succeed for a @IgnoreCase column directive" in {
      val schema = """@TotalColumns 1
                      column1: @IgnoreCase"""

      parse(new StringReader(schema)) must beLike { case Success(Schema(_, List(ColumnDefinition("column1", _, List(IgnoreCase())))),_) => ok }
    }
  }

  "Schema ordering" should {
    "allow any ordering of column directives - optional before ignore case" in {
      val schema = """@TotalColumns 1
                      column1: @Optional @IgnoreCase"""

      parse(new StringReader(schema)) must beLike { case Success(Schema(_, List(ColumnDefinition("column1", _, Optional() :: IgnoreCase() :: Nil))),_) => ok }
    }

    "allow any ordering of column directives - ignore case before optional" in {
      val schema = """@TotalColumns 1
                      column1: @IgnoreCase @Optional"""

      parse(new StringReader(schema)) must beLike { case Success(Schema(_, List(ColumnDefinition("column1", _, IgnoreCase() :: Optional() :: Nil))),_) => ok }
    }
  }
}