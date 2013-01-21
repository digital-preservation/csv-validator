package uk.gov.tna.dri.schema

import org.specs2.mutable._
import uk.gov.tna.dri.io.Resource

/**
 * Unit test of SchemaParser interacting with internal/test resource .
 * @author David Ainslie
 */
class SchemaParserSpec extends Specification {
  "Mandatory fields" should { // TODO "totalColumns" is not actually manadatory and "name" is fabrication for a quick test
    "include: 'name', 'totalColumns'" in {
      val resource = new Resource[String]{def contents = List("totalColumns = 10", "name = Scooby Doo").iterator}
      SchemaParser.parse(resource) mustEqual Schema("Scooby Doo", 10)
    }

    "throw an exception when missing 'name'" in {
      val resource = new Resource[String]{def contents = List("totalColumns = 10").iterator}
      SchemaParser.parse(resource) must throwA[NoSuchElementException]
    }

    "throw an exception when missing 'totalColumns'" in {
      val resource = new Resource[String]{def contents = List("name = Scooby Doo").iterator}
      SchemaParser.parse(resource) must throwA[NoSuchElementException]
    }
  }

  "totalColumns" should {
    "be a positive integer" in {
      val resource = new Resource[String]{def contents = List("totalColumns = 10", "name = Scooby Doo").iterator}
      SchemaParser.parse(resource) mustEqual Schema("Scooby Doo", 10)
    }
  }
}

/**
 * Example of a Resource of String
 * @author David Ainslie
 */
class StringResource extends Resource[String] {
  /**
   * Contents where each given element (string) of the list can be thought of as a line in a text file.
   * @return Iterator[String] the contents of a list of String to iterate over.
   */
  def contents = List("", "").iterator
}

