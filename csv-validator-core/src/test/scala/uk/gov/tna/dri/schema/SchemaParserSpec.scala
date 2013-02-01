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

  "regex" should {

    "succeed for valid regex" in {
      regex must succeedOn("""regex "[0-9]"""")
    }

    "fail for an invalid regex" in {
      regex("""regex "[0-9"""") must beLike { case n: NoSuccess => n.msg mustEqual "regex invalid: [0-9" }
    }

    "fail for missing value in regex" in {
      regex must failOn("regex").withMsg("Invalid regex rule")
    }
  }

  "Column definition" should  {

    "succeed for no rules" in {
      column must succeedOn(""""Last Name"""")
    }

    "succeed with single regex rule" in {
      column must succeedOn(""""Last Name" regex "[0-9]"""")
    }

    "fail if there is a regex rule but no column identifier" in {
      column must failOn(""""regex "[0-9]"""")
    }
  }

  "Schema" should {

    "fail for missing @TotalColumns" in {
      parse(new StringReader("rubbish")) must beLike {
        case Failure(message, next) => (message mustEqual "@TotalColumns invalid") and (next.pos.line mustEqual 1) and (next.pos.column mustEqual 1)
      }
    }

    "succeed for valid minimal schema" in {
      val colDefs = List(new ColumnDefinition("column1"),new ColumnDefinition("column2"),new ColumnDefinition("column3"))
      parse(new StringReader(
        """@TotalColumns 3
          |"column1"
          |"column2"
          |"column3"
        """.stripMargin)) must beLike { case Success(schema, _) => schema mustEqual Schema(3,colDefs) }
    }

    "succeed for valid schema with single rule" in {
      val schema = """@TotalColumns 1
                     "Last Name" regex "[a]""""
      parse(new StringReader(schema)) must beLike { case Success(Schema(1, List(ColumnDefinition("Last Name", List(RegexRule(r))))), _) => r.pattern.pattern mustEqual "[a]" }
    }

    "fail if there are more than 1 regex rules with *****HORRIBLE MESSAGE*****" in {
      val schema = """@TotalColumns 1
                     "Last Name" regex "[a]" regex "[0-5]""""

      parse(new StringReader(schema)) must beLike { case Failure(message, next) => (message mustEqual "string matching regex `\\z' expected but `r' found") }
    }

    "fail if the total number of columns does not match the number of column definitions" in {
      val schema = """@TotalColumns 2
                     "Last Name" regex "[a]""""
      parse(new StringReader(schema)) must beLike { case Failure(message,_) => message mustEqual "Schema Invalid as @TotalColumns = 2 but number of columns defined = 1" }
    }
//
//    "fail for more than one column definition on a line" in {
//      val schema = """@TotalColumns 5
//                     "Last Name" regex "[a-z]*" "Age""""
//      parse(new StringReader(schema)) must beLike { case Failure(_, _) => ok }
//    }
  }
}
