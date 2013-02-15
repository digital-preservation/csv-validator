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

      parse(new StringReader(schema)) must beLike { case Failure(message, _) => message mustEqual "@TotalColumns = 2 but number of columns defined = 1" }
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

      parse(new StringReader(schema)) must beLike { case Success(Schema(1, List(ColumnDefinition("Age", List(RegexRule(Literal(Some(r)))), _))), _) => r mustEqual "[1-9]*" }
    }

    "fail for more than one column definition on a line" in {
      val schema = """@TotalColumns 1
                      LastName: regex ("[a-z]*") Age"""

      parse(new StringReader(schema)) must beLike { case Failure(message, _) => message mustEqual """Column definition contains invalid text""" }
    }

    "fail for extra text after column definition on a line" in {
      val schema = """@TotalColumns 3
                      LastName: regex ("[a-z]*")
                      FirstName: dfsdfsdfwe
                      Age:"""

      parse(new StringReader(schema)) must beLike { case Failure(message, _) => message mustEqual "Column definition contains invalid text" }
    }

    "fail for invalid column identifier as 'stripMargin' just to prove that only numbers, letters and underscore are allowed as part of a column identifier" in {
      val schema = """@TotalColumns 2
                      |Name :"""

      parse(new StringReader(schema)) must beLike {
        case Failure(message, _) => message mustEqual "Column identifier invalid"
      }
    }

    /*"fail when one invalid column reference" in {
      val schema ="""@TotalColumns 2
                     Column1: in($NotAColumn)
                     Column2:"""

      parse(new StringReader(schema)) must beLike {
        case Failure(message, _) => message mustEqual "Column: Column1 has invalid cross reference in: NotAColumn"
      }
    }

    "fail when there are two rules and one is invalid" in {
      val schema ="""@TotalColumns 2
                     Column1: in($Column2) in($NotAColumn2)
                     Column2:"""

      parse(new StringReader(schema)) must beLike {
        case Failure(message, _) => message mustEqual "Column: Column1 has invalid cross reference in: NotAColumn2"
      }
    }

    "fail when two rules are invalid " in {
      val schema ="""@TotalColumns 2
                     Column1: in($NotAColumn1) in($NotAColumn2)
                     Column2:"""

      parse(new StringReader(schema)) must beLike {
        case Failure(message, _) => message mustEqual "Column: Column1 has invalid cross reference in: NotAColumn1, in: NotAColumn2"
      }
    }

    "fail when two columns have two rules and each has one invalid column" in {
      val schema ="""@TotalColumns 2
                     Column1: in($Column2) in($NotAColumn2)
                     Column2: in($NotAColumn3) in($Column2)"""

      parse(new StringReader(schema)) must beLike {
        case Failure(message, _) => message mustEqual "Column: Column1 has invalid cross reference in: NotAColumn2\nColumn: Column2 has invalid cross reference in: NotAColumn3"
      }
    }

    "fail when two columns have two rules and each has one invalid column with diffferent rules" in {
      val schema ="""@TotalColumns 2
                     Column1: is($Column1) is($NotAColumn1)
                     Column2: not($Column2) not($NotAColumn2)
                     Column3: in($Column3) in($NotAColumn3)
                     Column4: starts($Column4) starts($NotAColumn4)
                     Column5: ends($Column5) ends($NotAColumn5)"""

      parse(new StringReader(schema)) must beLike {
        case Failure(message, _) => message mustEqual """@TotalColumns = 2 but number of columns defined = 5
                                                        |Column: Column1 has invalid cross reference is: NotAColumn1
                                                        |Column: Column2 has invalid cross reference not: NotAColumn2
                                                        |Column: Column3 has invalid cross reference in: NotAColumn3
                                                        |Column: Column4 has invalid cross reference starts: NotAColumn4
                                                        |Column: Column5 has invalid cross reference ends: NotAColumn5""".stripMargin
      }
    }

    "multi columns with same name is valid - TODO NOT CORRECT" in {
      val schema = """@TotalColumns 2
                      Column1:
                      Column1:"""

      parse(new StringReader(schema)) must beLike { case Success(schema, _) => schema mustEqual Schema(2, List(ColumnDefinition("Column1"), ColumnDefinition("Column1"))) }
    }

    "succeed if Column1 correctly has InRule that points to Column2" in {
      val schema = """@TotalColumns 2
                      Column1: in($Column2)
                      Column2:"""

      parse(new StringReader(schema)) must beLike {
        case Success(schema, _) => schema mustEqual Schema(2, List(ColumnDefinition("Column1", List(InRule(ColumnReference("Column2")))),
                                                                   ColumnDefinition("Column2")))
      }
    }*/
  }
}