package uk.gov.tna.dri.schema

import org.specs2.mutable._
import org.specs2.matcher.ParserMatchers
import java.io.StringReader

class SchemaParserColumnDefinitionsSpec extends Specification with ParserMatchers {

  object TestSchemaParser extends SchemaParser

  override val parsers = TestSchemaParser

  import TestSchemaParser._

  "Schema" should {

    "fail if the total number of columns does not match the number of column definitions" in {
      val schema = """@TotalColumns 2
                      LastName: regex ("[a]")"""

      parse(new StringReader(schema)) must beLike { case Failure(message, _) => message mustEqual "Schema invalid as @TotalColumns = 2 but number of columns defined = 1" }
    }

    "fail for invalid column identifier" in {
      val schema = """@TotalColumns 1
                      Last Name """

      parse(new StringReader(schema)) must beLike { case Failure(message, _) => message mustEqual "`:' expected but `N' found" }
    }

    "succeed for column definition with no rules" in {
      val schema = """@TotalColumns 1
                      Name:"""

      parse(new StringReader(schema)) must beLike { case Success(schema, _) => schema mustEqual Schema(1, List(ColumnDefinition("Name"))) }
    }

    "succeed for column definition with single regex rule" in {
      val schema = """@TotalColumns 1
                      Age: regex ("[1-9]*")"""

      parse(new StringReader(schema)) must beLike { case Success(Schema(1, List(ColumnDefinition("Age", List(RegexRule(r))))), _) => r.pattern.pattern mustEqual "[1-9]*" }
    }

    "fail for more than one column definition on a line" in {
      val schema = """@TotalColumns 1
                      LastName: regex ("[a-z]*") Age"""

      parse(new StringReader(schema)) must beLike { case Failure(message, _) => message mustEqual """Column definition contains invalid (extra) text""" }
    }

    "fail for extra text after column definition on a line" in {
      val schema = """@TotalColumns 3
                      LastName: regex ("[a-z]*")
                      FirstName: dfsdfsdfwe
                      Age:"""

      parse(new StringReader(schema)) must beLike { case Failure(message, _) => message mustEqual "Column definition contains invalid (extra) text" }
    }

    "fail for invalid column identifier as 'stripMargin' just to prove that only numbers, letters and underscore are allowed as part of a column identifier" in {
      val schema = """@TotalColumns 2
                      |Name :"""

      parse(new StringReader(schema)) must beLike {
        case Failure(message, _) => message mustEqual "Column identifier invalid"
      }
    }

  }
}
