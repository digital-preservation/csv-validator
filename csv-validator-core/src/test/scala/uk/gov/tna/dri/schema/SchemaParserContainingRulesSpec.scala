package uk.gov.tna.dri.schema

import org.specs2.mutable._
import org.specs2.matcher.ParserMatchers
import java.io.StringReader

class SchemaParserContainingRulesSpec extends Specification with ParserMatchers {

  object TestSchemaParser extends SchemaParser

  override val parsers = TestSchemaParser

  import TestSchemaParser._

  "Schema" should {

    "succeed for cross reference in rule" in {
      val schema = """@totalColumns 2
                      Name: in($FullName)
                      FullName:"""

      parse(new StringReader(schema)) must beLike {
        case Success(Schema(globalDirsOne, List(ColumnDefinition("Name", List(InRule(ColumnReference("FullName"))), _), _)), _)  => ok
      }
    }

    "succeed for regex and inRule rules on a single column" in {
      val schema = """@totalColumns 1
                      Name: regex ("[1-9][a-z]*") in("dog")"""

      parse(new StringReader(schema)) must beLike {
        case Success(Schema(globalDirsOne, List(ColumnDefinition("Name", List(RegexRule(Literal(Some(r))), InRule(Literal(Some(ir)))), _))), _) => {
          r mustEqual "[1-9][a-z]*"
          ir mustEqual "dog"
        }
      }
    }

    "succeed for inRule regex rules on a single and inRule has column reference and rules have had their order changed" in {
      val schema = """@totalColumns 1
                      Name: in($Name) regex ("[1-9][a-z]*")"""

      parse(new StringReader(schema)) must beLike {
        case Success(Schema(globalDirsOne, List(ColumnDefinition("Name", List(InRule(ColumnReference(ir)), RegexRule(Literal(Some(r)))), _))), _) => {
          r mustEqual "[1-9][a-z]*"
          ir mustEqual "Name"
        }
      }
    }

    "succeed for 'is' text rule" in {
      val schema = """@totalColumns 1
                      Country: is("UK")"""

      parse(new StringReader(schema)) must beLike {
        case Success(Schema(_, List(ColumnDefinition("Country", List(IsRule(Literal(Some("UK")))), _))), _) => ok
      }
    }

    "succeed for 'is' cross reference rule" in {
      val schema = """@totalColumns 2
                      Country: is($MyCountry)
                      MyCountry:"""

      parse(new StringReader(schema)) must beLike {
        case Success(Schema(_,
                            List(ColumnDefinition("Country", List(IsRule(ColumnReference("MyCountry"))), _),
                                 ColumnDefinition("MyCountry", _, _))),
                     _) => ok
      }
    }

    "fail for invalid 'is' cross reference rule" in {
      val schema = """@totalColumns 2
                     |Country: is($MyMissingCountry)
                     |MyCountry:""".stripMargin

      parse(new StringReader(schema)) must beLike { case Failure("Column: Country has invalid cross reference is($MyMissingCountry) at line: 2, column: 10", _) => ok }

    }

    "succeed for 'starts' text rule" in {
      val schema = """@totalColumns 1
                      Country: starts("United")"""

      parse(new StringReader(schema)) must beLike {
        case Success(Schema(_, List(ColumnDefinition("Country", List(StartsRule(Literal(Some("United")))), _))), _) => ok
      }
    }

    "succeed for 'starts' cross reference rule" in {
      val schema = """@totalColumns 2
                      Country: starts($MyCountry)
                      MyCountry:"""

      parse(new StringReader(schema)) must beLike {
        case Success(Schema(_,
                            List(ColumnDefinition("Country", List(StartsRule(ColumnReference("MyCountry"))), _),
                                 ColumnDefinition("MyCountry", _, _))),
                     _) => ok
      }
    }

    "fail for invalid 'starts' cross reference rule" in {
      val schema = """@totalColumns 2
                     |Country: starts($MyMissingCountry)
                     |MyCountry:""".stripMargin

      parse(new StringReader(schema)) must beLike { case Failure("Column: Country has invalid cross reference starts($MyMissingCountry) at line: 2, column: 10", _) => ok }

    }

    "succeed for 'ends' text rule" in {
      val schema = """@totalColumns 1
                      Country: ends("Kingdom")"""

      parse(new StringReader(schema)) must beLike {
        case Success(Schema(_, List(ColumnDefinition("Country", List(EndsRule(Literal(Some("Kingdom")))), _))), _) => ok
      }
    }

    "succeed for 'ends' cross reference rule" in {
      val schema = """@totalColumns 2
                      Country: ends($MyCountry)
                      MyCountry:"""

      parse(new StringReader(schema)) must beLike {
        case Success(Schema(_,
                            List(ColumnDefinition("Country", List(EndsRule(ColumnReference("MyCountry"))), _),
                                 ColumnDefinition("MyCountry", _, _))),
                     _) => ok
      }
    }

    "fail for invalid 'ends' cross reference rule" in {
      val schema = """@totalColumns 2
                     |Country: ends($MyMissingCountry)
                     |MyCountry:""".stripMargin

      parse(new StringReader(schema)) must beLike { case Failure("Column: Country has invalid cross reference ends($MyMissingCountry) at line: 2, column: 10", _) => ok }

    }

    "succeed for 'isNot' text rule" in {
      val schema = """@totalColumns 1
                      Country: isNot("USA")"""

      parse(new StringReader(schema)) must beLike {
        case Success(Schema(_, List(ColumnDefinition("Country", List(IsNotRule(Literal(Some("USA")))), _))), _) => ok
      }
    }

    "succeed for 'isNot' cross reference rule" in {
      val schema = """@totalColumns 2
                      Country: isNot($MyCountry)
                      MyCountry:"""

      parse(new StringReader(schema)) must beLike {
        case Success(Schema(_,
                            List(ColumnDefinition("Country", List(IsNotRule(ColumnReference("MyCountry"))), _),
                                 ColumnDefinition("MyCountry", _, _))),
                     _) => ok
      }
    }

    "fail for invalid 'isNot' cross reference rule" in {
      val schema = """@totalColumns 2
                     |Country: isNot($MyMissingCountry)
                     |MyCountry:""".stripMargin

      parse(new StringReader(schema)) must beLike { case Failure("Column: Country has invalid cross reference isNot($MyMissingCountry) at line: 2, column: 10", _) => ok }

    }
  }
}