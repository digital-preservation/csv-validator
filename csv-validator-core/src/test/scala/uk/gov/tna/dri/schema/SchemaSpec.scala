package uk.gov.tna.dri.schema

import org.specs2.mutable.Specification
import com.sun.org.apache.xerces.internal.impl.xs.opti.SchemaDOMParser

class SchemaSpec extends Specification {

  "Total columns" should {

    "be a positive integer" in {

      SchemaParser.parse("@TotalColumns : 5") mustEqual Schema("5")
      SchemaParser.parse("@TotalColumns : 0") mustEqual Schema("0")
      SchemaParser.parse("@TotalColumns : -5") mustEqual "string matching regex `\\d+' expected but `-' found at line: 1, column: 17"
    }
  }
}
