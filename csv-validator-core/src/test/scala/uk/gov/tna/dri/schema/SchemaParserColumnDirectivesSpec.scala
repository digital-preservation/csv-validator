package uk.gov.tna.dri.schema

import org.specs2.mutable._
import org.specs2.matcher.ParserMatchers
import java.io.StringReader

class SchemaParserColumnDirectivesSpec extends Specification with ParserMatchers {

  object TestSchemaParser extends SchemaParser

  override val parsers = TestSchemaParser

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

      parse(new StringReader(schema)) must beLike { case Failure(message, _) => (message mustEqual """column1: Duplicated column directives: @ignoreCase at line: 3, column: 10""") }
    }

    "fail for multiple duplicate column directives" in {
      val schema = """version 1.0
                     |@totalColumns 1
                     |column1: @ignoreCase @optional @ignoreCase @optional""".stripMargin

      parse(new StringReader(schema)) must beLike { case Failure(message, _) => (message mustEqual """column1: Duplicated column directives: @ignoreCase at line: 3, column: 10, @optional at line: 3, column: 22""") }
    }

    "fail for duplicate column directives on different columns" in {
      val schema = """version 1.0
                     |@totalColumns 3
                     |column1: @ignoreCase @optional @ignoreCase @optional
                     |column2: @optional @ignoreCase
                     |column3: @ignoreCase @ignoreCase @optional @optional""".stripMargin

      parse(new StringReader(schema)) must beLike { case Failure(message, _) => (message mustEqual
        """column1: Duplicated column directives: @ignoreCase at line: 3, column: 10, @optional at line: 3, column: 22
          |column3: Duplicated column directives: @ignoreCase at line: 5, column: 10, @optional at line: 5, column: 34""".stripMargin) }
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