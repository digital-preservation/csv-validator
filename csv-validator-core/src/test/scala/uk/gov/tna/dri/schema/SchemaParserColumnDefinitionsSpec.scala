package uk.gov.tna.dri.schema

import org.specs2.mutable._
import org.specs2.matcher.ParserMatchers
import java.io.StringReader
import scala.Some

class SchemaParserColumnDefinitionsSpec extends Specification with ParserMatchers {

  object TestSchemaParser extends SchemaParser

  override val parsers = TestSchemaParser

  import TestSchemaParser._

  "Schema" should {
    val globalDirsOne = List(TotalColumns(1))
    val globalDirsTwo = List(TotalColumns(2))

    "succeed for valid schema with all possible column definitions" in {
      val columnDefinitions = List(new ColumnDefinition("column1"),new ColumnDefinition("column2"),new ColumnDefinition("column3"),
        new ColumnDefinition("."),new ColumnDefinition("_-co.l"),new ColumnDefinition("0.a-B-z_Z"),new ColumnDefinition("-abc.txt"))

      val schema = s"""version ${Schema.SchemaVersion}
                      @totalColumns 7
                      column1:
                      column2:
                      column3:
                      .:
                      _-co.l:
                      0.a-B-z_Z:
                      -abc.txt:"""

      parse(new StringReader(schema)) must beLike { case Success(schema, _) => schema mustEqual Schema(List(TotalColumns(7)), columnDefinitions) }
    }

    "fail if colunm ident contains an in valid char ie not 0-9 a-z A-Z . - _" in {
      val schema = s"""version ${Schema.SchemaVersion}
      @totalColumns 1
      column1':"""
      parse(new StringReader(schema)) must beLike {
        case Failure(messages, _) => messages mustEqual "Column definition contains invalid text"
      }
    }

    "fail if the total number of columns does not match the number of column definitions" in {
      val schema = s"""version ${Schema.SchemaVersion}
                      |@totalColumns 2
                      |LastName: regex ("[a]")""".stripMargin

      parse(new StringReader(schema)) must beLike { case Failure(message, _) => message mustEqual "@totalColumns = 2 but number of columns defined = 1 at line: 2, column: 1" }
    }

    "fail for invalid column identifier" in {
      val schema = s"""version ${Schema.SchemaVersion}
                      @totalColumns 1
                      Last Name """

      parse(new StringReader(schema)) must beLike { case Failure(message, _) => message mustEqual "Column definition contains invalid text" }
    }

    "succeed for column definition with no rules" in {
      val schema = s"""version ${Schema.SchemaVersion}
                      @totalColumns 1
                      Name:"""

      parse(new StringReader(schema)) must beLike { case Success(schema, _) => schema mustEqual Schema(globalDirsOne, List(ColumnDefinition("Name"))) }
    }

    "succeed for column definition with single regex rule" in {
      val schema = s"""version ${Schema.SchemaVersion}
                      @totalColumns 1
                      Age: regex ("[1-9]*")"""

      parse(new StringReader(schema)) must beLike { case Success(Schema(globalDirsOne, List(ColumnDefinition("Age", List(RegexRule(Literal(Some(r)))), _))), _) => r mustEqual "[1-9]*" }
    }

    "fail for more than one column definition on a line" in {
      val schema = s"""version ${Schema.SchemaVersion}
                      @totalColumns 1
                      LastName: regex ("[a-z]*") Age"""

      parse(new StringReader(schema)) must beLike { case Failure(message, _) => message mustEqual """Column definition contains invalid text""" }
    }

    "fail for extra text after column definition on a line" in {
      val schema = s"""version ${Schema.SchemaVersion}
                      @totalColumns 3
                      LastName: regex ("[a-z]*")
                      FirstName: dfsdfsdfwe
                      Age:"""

      parse(new StringReader(schema)) must beLike { case Failure(message, _) => message mustEqual "Column definition contains invalid text" }
    }

    "fail when one invalid column reference" in {
      val schema = s"""version ${Schema.SchemaVersion}
                     @totalColumns 2
                    |Column1: in($$NotAColumn)
                    |Column2:""".stripMargin

      parse(new StringReader(schema)) must beLike {
        case Failure(message, _) => message mustEqual "Column: Column1 has invalid cross reference in($NotAColumn) at line: 3, column: 10"
      }
    }

    "fail when there are two rules and one is invalid" in {
      val schema = s"""version ${Schema.SchemaVersion}
                     @totalColumns 2
                    |Column1: in($$Column2) in($$NotAColumn2)
                    |Column2:""".stripMargin

      parse(new StringReader(schema)) must beLike {
        case Failure(message, _) => message mustEqual "Column: Column1 has invalid cross reference in($NotAColumn2) at line: 3, column: 23"
      }
    }

    "fail when two columns have two rules and each has one invalid column" in {
      val schema =s"""version ${Schema.SchemaVersion}
                     @totalColumns 2
                    |Column1: in($$Column2) in($$NotAColumn2)
                    |Column2: in($$NotAColumn3) in($$Column2)""".stripMargin

      parse(new StringReader(schema)) must beLike {
        case Failure(message, _) => message mustEqual
          """Column: Column1 has invalid cross reference in($NotAColumn2) at line: 3, column: 23
            |Column: Column2 has invalid cross reference in($NotAColumn3) at line: 4, column: 10""".stripMargin
      }
    }

    "fail when two columns have two rules and each has one invalid column with different rules" in {
      val schema =s"""version ${Schema.SchemaVersion}
                    |@totalColumns 2
                    |Column1: is($$Column1) is($$NotAColumn1)
                    |Column2: isNot($$Column2) isNot($$NotAColumn2)
                    |Column3: in($$Column3) in($$NotAColumn3)
                    |Column4: starts($$Column4) starts($$NotAColumn4)
                    |Column5: ends($$Column5) ends($$NotAColumn5)""".stripMargin

      parse(new StringReader(schema)) must beLike {
        case Failure(message, _) => message mustEqual """@totalColumns = 2 but number of columns defined = 5 at line: 2, column: 1
                                                        |Column: Column1 has invalid cross reference is($NotAColumn1) at line: 3, column: 23
                                                        |Column: Column2 has invalid cross reference isNot($NotAColumn2) at line: 4, column: 26
                                                        |Column: Column3 has invalid cross reference in($NotAColumn3) at line: 5, column: 23
                                                        |Column: Column4 has invalid cross reference starts($NotAColumn4) at line: 6, column: 27
                                                        |Column: Column5 has invalid cross reference ends($NotAColumn5) at line: 7, column: 25""".stripMargin
      }
    }

    "fail for multiple columns with same name" in {
      val schema = s"""version ${Schema.SchemaVersion}
                      @totalColumns 4
                      Column1:
                      Column2:
                      Column1: regex("A")
                      Column2:"""

      parse(new StringReader(schema)) must beLike {
        case Failure(errors, _) => errors mustEqual """Column: Column1 has duplicates on lines 3, 5
                                                      |Column: Column2 has duplicates on lines 4, 6""".stripMargin
      }
    }

    "succeed if Column1 correctly has InRule that points to Column2" in {
      val schema = s"""version 1.0
                      @totalColumns 2
                      Column1: in($$Column2)
                      Column2:"""

      parse(new StringReader(schema)) must beLike {
        case Success(schema, _) => schema mustEqual Schema(globalDirsTwo, List(ColumnDefinition("Column1", List(InRule(ColumnReference("Column2")))),
                                                                               ColumnDefinition("Column2")))
      }
    }

    "fail for invalid column cross references" in {
      val schema =s"""version ${Schema.SchemaVersion}
                     @totalColumns 2
                    |Age: in($$Blah) regex ("[0-9]+")
                    |Country: in($$Boo)""".stripMargin

      parse(new StringReader(schema)) must beLike {
        case Failure(message, _) => message mustEqual
          """Column: Age has invalid cross reference in($Blah) at line: 3, column: 6
            |Column: Country has invalid cross reference in($Boo) at line: 4, column: 10""".stripMargin
      }
    }
  }
}