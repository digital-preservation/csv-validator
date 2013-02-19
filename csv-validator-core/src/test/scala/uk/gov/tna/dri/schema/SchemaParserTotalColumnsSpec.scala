package uk.gov.tna.dri.schema

import org.specs2.mutable._
import org.specs2.matcher.ParserMatchers
import java.io.StringReader

class SchemaParserTotalColumnsSpec extends Specification with ParserMatchers {

  object TestSchemaParser extends SchemaParser

  override val parsers = TestSchemaParser

  import TestSchemaParser._

  "Schema" should {

    "fail for TotalColumns with missing value" in {
      parse(new StringReader("@TotalColumns")) must beLike { case Failure(message, _) => message mustEqual "@TotalColumns invalid" }
    }

    "fail for incorrect TotalColumns field name" in {
      parse(new StringReader("@ToalColumns 23")) must beLike { case Failure(message, _) => message mustEqual "Global directives contains invalid text" }
    }

    "fail for incorrect TotalColumns field name with no value" in {
      parse(new StringReader("@TtalColumns")) must beLike { case Failure(message, _) => message mustEqual "Global directives contains invalid text" }
    }

    "fail for TotalColumns field name incorrect case" in {
      parse(new StringReader("@totalColumns 65")) must beLike { case Failure(message, _) => message mustEqual "Global directives contains invalid text" }
    }

    "fail for TotalColumns of zero" in {
      parse(new StringReader("@TotalColumns 0")) must beLike { case Failure(message, _) => message mustEqual "@TotalColumns invalid" }
    }

    "fail for TotalColumns with negative integer" in {
      parse(new StringReader("@TotalColumns -23")) must beLike { case Failure(message, _) => message mustEqual "@TotalColumns invalid" }
    }

    "fail for TotalColumns with non integer" in {
      parse(new StringReader("@TotalColumns 132.45")) must beLike { case Failure(message, _) => message mustEqual "Global directives contains invalid text" }
    }

    "fail for TotalColumns with non numeric" in {
      parse(new StringReader("@TotalColumns blah")) must beLike { case Failure(message, _) => message mustEqual "@TotalColumns invalid" }
    }
  }
}