package uk.gov.tna.dri.schema

import org.specs2.mutable._
import org.specs2.matcher.ParserMatchers
import java.io.StringReader

class SchemaParserSpec extends Specification with ParserMatchers {

  object TestSchemaParser extends SchemaParser

  override val parsers = TestSchemaParser

  import TestSchemaParser._


  "Schema" should {

    "fail for TotalColumns with missing value" in {
      parse(new StringReader("@TotalColumns")) must beLike { case Failure(message, _) => message mustEqual "@TotalColumns invalid" }
    }

    "fail for incorrect TotalColumns field name" in {
      parse(new StringReader("@ToalColumns 23")) must beLike { case Failure(message, _) => message mustEqual "@TotalColumns invalid" }
    }

    "fail for incorrect TotalColumns field name with no value" in {
      parse(new StringReader("@TtalColumns")) must beLike { case Failure(message, _) => message mustEqual "@TotalColumns invalid" }
    }

    "fail for TotalColumns field name incorrect case" in {
      parse(new StringReader("@totalColumns 65")) must beLike { case Failure(message, _) => message mustEqual "@TotalColumns invalid" }
    }

    "fail for TotalColumns of zero" in {
      parse(new StringReader("@TotalColumns 0")) must beLike { case Failure(message, _) => message mustEqual "@TotalColumns invalid" }
    }

    "fail for TotalColumns with negative integer" in {
      parse(new StringReader("@TotalColumns -23")) must beLike { case Failure(message, _) => message mustEqual "@TotalColumns invalid" }
    }

    "fail for TotalColumns with non integer" in {
      parse(new StringReader("@TotalColumns 132.45")) must beLike { case Failure(message, _) => message mustEqual "@TotalColumns invalid" }
    }

    "fail for TotalColumns with non numeric" in {
      parse(new StringReader("@TotalColumns blah")) must beLike { case Failure(message, _) => message mustEqual "@TotalColumns invalid" }
    }

    "succeed for valid minimal schema" in {
      val colDefs = List(new ColumnDefinition("column1"),new ColumnDefinition("column2"),new ColumnDefinition("column3"))

      val schema = """@TotalColumns 3
                      "column1"
                      "column2"
                      "column3""""

      parse(new StringReader(schema)) must beLike { case Success(schema, _) => schema mustEqual Schema(3,colDefs) }
    }

    "succeed for valid regex rule" in {
      val schema = """@TotalColumns 1
                     "Last Name" regex "[a]""""
      parse(new StringReader(schema)) must beLike { case Success(Schema(1, List(ColumnDefinition("Last Name", List(RegexRule(r))))), _) => r.pattern.pattern mustEqual "[a]" }
    }

    "fail for an invalid regex" in {
      val schema = """@TotalColumns 1
                     "Something" regex "[0-9""""

      parse(new StringReader(schema)) must beLike { case Failure(message, next) => (message mustEqual "regex invalid: [0-9") }
    }

//    "fail for missing value in regex" in {
//      val schema = """@TotalColumns 1
//                     "Something" regex"""
//
//      parse(new StringReader(schema)) must beLike { case Failure(message, next) => (message mustEqual "regex rule invalid") }
//    }
//
//    "fail if there are more than 1 regex rules with *****HORRIBLE MESSAGE*****" in {
//      val schema = """@TotalColumns 1
//                     "Last Name" regex "[a]" regex "[0-5]""""
//
//      parse(new StringReader(schema)) must beLike { case Failure(message, next) => (message mustEqual "Column definition invalid") }
//    }

    "fail if the total number of columns does not match the number of column definitions" in {
      val schema = """@TotalColumns 2
                     "Last Name" regex "[a]""""
      parse(new StringReader(schema)) must beLike { case Failure(message, _) => message mustEqual "Schema invalid as @TotalColumns = 2 but number of columns defined = 1" }
    }

    "fail for invalid column identifier" in {
      val schema = """@TotalColumns 1
                     Last Name """
      parse(new StringReader(schema)) must beLike { case Failure(message, _) => message mustEqual "Column definition invalid" }
    }

    "succeed for column definition with no rules" in {
      val schema = """@TotalColumns 1
                      "Name""""

      parse(new StringReader(schema)) must beLike { case Success(schema, _) => schema mustEqual Schema(1, List(ColumnDefinition("Name"))) }
    }

    "succeed for column definition with single regex rule" in {
      val schema = """@TotalColumns 1
                      "Age" regex "[1-9]*""""

      parse(new StringReader(schema)) must beLike { case Success(Schema(1, List(ColumnDefinition("Age", List(RegexRule(r))))), _) => r.pattern.pattern mustEqual "[1-9]*" }
    }

    "fail for more than one column definition on a line with *****HORRIBLE MESSAGE*****" in {
      val schema = """@TotalColumns 1
                     "Last Name" regex "[a-z]*" "Age""""

      parse(new StringReader(schema)) must beLike { case Failure(message, _) => message mustEqual """string matching regex `\z' expected but `"' found""" }
    }

    "fail for extra text after column definition on a line" in {
      val schema = """@TotalColumns 3
                     "Last Name" regex "[a-z]*"
                     "First Name" dfsdfsdfwe
                     "Age""""

      parse(new StringReader(schema)) must beLike { case Failure(message, _) => message mustEqual "regex rule invalid" }
    }
  }
}
