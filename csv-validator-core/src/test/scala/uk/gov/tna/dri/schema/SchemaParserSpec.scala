package uk.gov.tna.dri.schema

import org.specs2.mutable._
import org.specs2.matcher.ParserMatchers
import java.io.StringReader

class SchemaParserSpec extends Specification with ParserMatchers {

  object TestSchemaParser extends SchemaParser

  override val parsers = TestSchemaParser

  import TestSchemaParser._

  "@TotalColumns" should {

    "fail for incorrect field name" in {
      totalColumns must failOn("@ToalColumns 23")
    }

    "fail for field name incorrect case" in {
      totalColumns must failOn("@totalColumns 65")
    }

    "fail for missing value" in {
      totalColumns must failOn("@TotalColumns ")
    }

    "succeed for integer > 0" in {
      totalColumns must succeedOn("@TotalColumns 5").withResult(5)
    }

    "allow whitespace between field and value" in {
      totalColumns must succeedOn("@TotalColumns    43").withResult(43)
    }

    "fail for zero" in {
      totalColumns must failOn("@TotalColumns 0")
    }

    "fail for negative integer" in {
      totalColumns must failOn("@TotalColumns -23")
    }

    "fail for non integer" in {
      totalColumns must failOn("@TotalColumns 132.45")
    }

    "fail for non numeric" in {
      totalColumns must failOn("@TotalColumns blah")
    }
  }

  "regex for a given column directive" should {
    "succeed for valid regex" in {
      regex must succeedOn(""" regex "[0-9]"""")
    }
  }

  "Column directive" should  {
    "fail for missing value in regex" in {
      column must failOn(""""Last Name" regex""")
    }

    "fail for an invalid regex" in {
      column must failOn(""""Last Name" regex "[0-9"""")
    }

    "succeed for a valid regex" in {
      column must succeedOn(""""Last Name" regex "[0-9]"""")
    }
  }

  "Schema" should {

    "fail for invalid schema" in {
      parse(new StringReader("rubbish")) must beLike { case Failure(message, next) => (message mustEqual "@TotalColumns invalid") and (next.pos.line mustEqual 1) and (next.pos.column mustEqual 1) }
    }

    "succeed for valid schema" in {
      parse(new StringReader("@TotalColumns 43")) must beLike { case Success(schema, _) => schema mustEqual Schema(43) }
    }

    "succeed for valid @TotalColumns and regex" in {
      parse(new StringReader(
        """
          @TotalColumns 5
          "Last Name" regex "[a]"
        """)) must beLike { case Success(Schema(5, List(ColumnDefinition(_, List(RegexRule(r))))), _) => r.pattern.pattern mustEqual "[a]" }
    }

    "fail for bad regex" in {

      parse(new StringReader(
        """
          @TotalColumns 5
          "Last Name" regex "[a"
        """)) must beLike { case Failure("regex invalid: [a", _) => ok}
    }
  }
}
