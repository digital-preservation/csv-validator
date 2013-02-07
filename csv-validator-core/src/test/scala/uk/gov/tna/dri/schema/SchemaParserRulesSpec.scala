package uk.gov.tna.dri.schema

import org.specs2.mutable._
import org.specs2.matcher.ParserMatchers
import java.io.StringReader

class SchemaParserRulesSpec extends Specification with ParserMatchers {

  object TestSchemaParser extends SchemaParser

  override val parsers = TestSchemaParser

  import TestSchemaParser._

  "Schema" should {

    "succeed for valid regex rule" in {
      val schema = """@TotalColumns 1
                      LastName: regex ("[a]")"""

      parse(new StringReader(schema)) must beLike { case Success(Schema(1, List(ColumnDefinition("LastName", List(RegexRule(r))))), _) => r.pattern.pattern mustEqual "[a]" }
    }

    "fail for an invalid regex" in {
      val schema = """@TotalColumns 1
                      Something: regex ("[0-9")"""

      parse(new StringReader(schema)) must beLike { case Failure(message, next) => (message mustEqual "regex invalid: [0-9") }
    }

    "fail for missing quotes defining a regex" in {
      val schema = """@TotalColumns 3
                      LastName:
                      FirstName: regex ("a)
                      Age:"""

      parse(new StringReader(schema)) must beLike {
        case Failure(message, _) => message mustEqual "regex not correctly delimited as (\"your regex\")"
      }
    }

    "fail for missing value in regex" in {
      val schema = """@TotalColumns 1
                      Something: regex"""

      parse(new StringReader(schema)) must beLike { case Failure(message, next) => (message mustEqual "regex not correctly delimited as (\"your regex\")") }
    }

    "fail if there is more than 1 regex rule" in {
      val schema = """@TotalColumns 1
                      LastName: regex ("[a]") regex ("[0-5]")"""

      parse(new StringReader(schema)) must beLike { case Failure(message, next) => (message mustEqual "regex invalid: [a]\") regex (\"[0-5]") }
    }

    "succeed for regex and inRule rules on a single column" in {
      val schema = """@TotalColumns 1
                      Name: regex ("[1-9][a-z]*") in(dog)"""

      parse(new StringReader(schema)) must beLike { case Success(Schema(1, List(ColumnDefinition("Name", List(RegexRule(r),InRule(ir))))), _) => {
        r.pattern.pattern mustEqual "[1-9][a-z]*"
        ir mustEqual LiteralTypeProvider("dog")
      } }
    }
  }
}
