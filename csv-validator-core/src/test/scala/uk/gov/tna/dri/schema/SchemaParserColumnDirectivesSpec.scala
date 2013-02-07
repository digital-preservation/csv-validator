package uk.gov.tna.dri.schema

import org.specs2.mutable._
import org.specs2.matcher.ParserMatchers
import java.io.StringReader

class SchemaParserColumnDirectivesSpec extends Specification with ParserMatchers {

  object TestSchemaParser extends SchemaParser

  override val parsers = TestSchemaParser

  import TestSchemaParser._

  "Schema" should {

    "succeed for an @Optional column directive" in {
      val schema = """@TotalColumns 1
                      column1: @Optional"""

      parse(new StringReader(schema)) must beLike { case Success(Schema(_,List(ColumnDefinition("column1", _, List(Optional())))),_) => ok }
    }
  }
}
