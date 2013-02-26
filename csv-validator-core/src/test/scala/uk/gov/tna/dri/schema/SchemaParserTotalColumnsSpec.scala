package uk.gov.tna.dri.schema

import org.specs2.mutable._
import java.io.StringReader

class SchemaParserTotalColumnsSpec extends Specification {

  object TestSchemaParser extends SchemaParser

  import TestSchemaParser._

  "Schema" should {

    "fail for TotalColumns with missing value" in {
      parse(new StringReader("version 1.0\n@totalColumns")) must beLike { case Failure(message, _) => message mustEqual "@totalColumns invalid" }
    }

    "fail for incorrect TotalColumns field name" in {
      parse(new StringReader("version 1.0\n@ToalColumns 23")) must beLike { case Failure(message, _) => message mustEqual "Global directives contains invalid text" }
    }

    "fail for incorrect TotalColumns field name with no value" in {
      parse(new StringReader("version 1.0\n@TtalColumn")) must beLike { case Failure(message, _) => message mustEqual "Global directives contains invalid text" }
    }

    "fail for TotalColumns field name incorrect case" in {
      parse(new StringReader("version 1.0\n@TotalColumns 65")) must beLike { case Failure(message, _) => message mustEqual "Global directives contains invalid text" }
    }

    "fail for TotalColumns of zero" in {
      parse(new StringReader("version 1.0\n@totalColumns 0")) must beLike { case Failure(message, _) => message mustEqual "@totalColumns invalid" }
    }

    "fail for TotalColumns with negative integer" in {
      parse(new StringReader("version 1.0\n@totalColumns -23")) must beLike { case Failure(message, _) => message mustEqual "@totalColumns invalid" }
    }

    "fail for TotalColumns with non integer" in {
      parse(new StringReader("version 1.0\n@totalColumns 132.45")) must beLike { case Failure(message, _) => message mustEqual "Global directives contains invalid text" }
    }

    "fail for TotalColumns with non numeric" in {
      parse(new StringReader("version 1.0\n@totalColumns blah")) must beLike { case Failure(message, _) => message mustEqual "@totalColumns invalid" }
    }
  }
}