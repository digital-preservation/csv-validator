package uk.gov.tna.dri.schema

import org.specs2.mutable._
import scala.io.Source
import uk.gov.tna.dri.io.Resource
import java.io.File

/**
 * Integration test of SchemaParser interacting with external resource.
 * @author David Ainslie
 */
class SchemaParserITSpec extends Specification {
  "Mandatory fields" should {
    "include: 'name', 'totalColumns'" in {
      val resource = new FileResource("src/test/resources/schema/csv-schema-usage.txt")
      SchemaParser.parse(resource) mustEqual Schema("Scooby Doo", 10)
    }
  }
}

/**
 * @author David Ainslie
 */
class FileResource(val file: File) extends Resource[String] {
  /** Given a path construct a File representing the path for this resource. */
  def this(path: String) = this(new File(path))

  /** Contents of a text file. */
  def contents = Source.fromFile(file).getLines
}

