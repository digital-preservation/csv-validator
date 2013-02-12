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
                      LastName: regex("[a]")"""

      parse(new StringReader(schema)) must beLike { case Success(Schema(1, List(ColumnDefinition("LastName", List(RegexRule(r)), _))), _) => r.pattern.pattern mustEqual "[a]" }
    }

    "fail for an invalid regex" in {
      val schema = """@TotalColumns 1
                      Something: regex ("[0-9")"""

      parse(new StringReader(schema)) must beLike { case Failure(message, _) => (message mustEqual """regex invalid: ("[0-9")""") }
    }

    "fail for missing quotes defining a regex" in {
      val schema = """@TotalColumns 3
                      LastName:
                      FirstName: regex ("a)
                      Age:"""

      parse(new StringReader(schema)) must beLike {
        case Failure(message, _) => message mustEqual """regex not correctly delimited as ("your regex")"""
      }
    }

    "fail for missing value in regex" in {
      val schema = """@TotalColumns 1
                      Something: regex"""

      parse(new StringReader(schema)) must beLike { case Failure(message, _) => (message mustEqual "regex not correctly delimited as (\"your regex\")") }
    }

    "succeed for more than 1 regex rule" in {
      val schema = """@TotalColumns 1
                      LastName: regex("[a]") regex("[0-5]")"""

      parse(new StringReader(schema)) must beLike { case Success(_, _) => ok }
    }

    "succeed for cross reference in rule" in {
      val schema = """@TotalColumns 2
                      Name: in("$FullName")
                      FullName:"""

      parse(new StringReader(schema)) must beLike {
        case Success(Schema(2, List(ColumnDefinition("Name", List(InRule(ColumnTypeProvider("FullName"))), _), _)), _)  => ok
      }
    }

    "succeed for regex and inRule rules on a single column" in {
      val schema = """@TotalColumns 1
                      Name: regex ("[1-9][a-z]*") in("dog")"""

      parse(new StringReader(schema)) must beLike {
        case Success(Schema(1, List(ColumnDefinition("Name", List(RegexRule(r), InRule(LiteralTypeProvider(ir))), _))), _) => {
          r.pattern.pattern mustEqual "[1-9][a-z]*"
          ir mustEqual "dog"
        }
      }
    }

    "succeed for inRule regex rules on a single and inRule has column reference and rules have had their order changed" in {
      val schema = """@TotalColumns 1
                      Name: in("$dog") regex ("[1-9][a-z]*")"""

      parse(new StringReader(schema)) must beLike {
        case Success(Schema(1, List(ColumnDefinition("Name", List(InRule(ColumnTypeProvider(ir)), RegexRule(r)), _))), _) => {
          r.pattern.pattern mustEqual "[1-9][a-z]*"
          ir mustEqual "dog"
        }
      }
    }

    "succeed for file exists rule" in {
      val schema = """@TotalColumns 1
                      Name: fileExists()"""
      parse(new StringReader(schema)) must beLike { case Success(Schema(1, List(ColumnDefinition("Name", List(FileExistsRule(None)), _))), _) => ok}
    }

    "succeed for file exists rule with root file path" in {
      val schema = """@TotalColumns 1
                      Name: fileExists("some/root/path")"""
      parse(new StringReader(schema)) must beLike { case Success(Schema(1, List(ColumnDefinition("Name", List(FileExistsRule(Some(rootPath))), _))), _) => rootPath mustEqual "some/root/path"}
    }

    "fail for non quoted root file path" in {
      val schema = """@TotalColumns 1
                      Name: fileExists(some/other/root/path)"""
      parse(new StringReader(schema)) must beLike { case Failure("Column definition contains invalid text", _) => ok}
    }

    "fail for non parentheses" in {
      val schema = """@TotalColumns 1
                      Name: fileExists /root/path"""
      parse(new StringReader(schema)) must beLike { case Failure("Column definition contains invalid text", _) => ok}
    }
  }
}