package uk.gov.tna.dri.validator

import org.specs2.mutable.Specification
import uk.gov.tna.dri.schema._
import java.io.{Reader, StringReader}
import scalaz.Success
import uk.gov.tna.dri.schema.Schema
import scalaz.Failure

class MetaDataValidatorChecksumSpec extends Specification {

  implicit def stringToStringReader(s: String): StringReader = new StringReader(s.replaceAll("\n\\s+", "\n"))

  implicit def stringToSchema(s: String): Schema = {
    val schemaParser = new SchemaParser() {
      override def parse(reader: Reader): ParseResult[Schema] = {
        super.parse(reader) match {
          case s @ Success(schema: Schema, _) => s
          case NoSuccess(message, next) => throw new RuntimeException(message)
        }
      }
    }

    schemaParser.parse(s).get
  }

  object TestMetaDataValidator extends AllErrorsMetaDataValidator

  import TestMetaDataValidator._

  "Validation" should {

    "succeed when calculated algorithm does match given cross referenced string value - with /" in {
      val schema =
        """@totalColumns 2 @noHeader
           File:
           MD5: checksum(file($File), "MD5")
        """

      val metaData = """src/test/resources/uk/gov/tna/dri/schema/checksum.txt,232762380299115da6995e4c4ac22fa2"""

      validate(metaData, schema) must beLike { case Success(_) => ok }
    }

    "fail when calculated algorithm does not match given cross referenced string value - with /" in {
      val schema =
        """@totalColumns 2 @noHeader
           File:
           MD5: checksum(file($File), "MD5")
        """

      val metaData = """src/test/resources/uk/gov/tna/dri/schema/checksum.txt,rubbish"""

      validate(metaData, schema) must beLike {
        case Failure(messages) => messages.list mustEqual List("checksum(file($File)) fails for line: 1, column: MD5, value: rubbish")
      }
    }

    "succeed when calculated algorithm does match given root & cross referenced string value - with /" in {
      val schema =
        """@totalColumns 2 @noHeader
           File:
           MD5: checksum(file("src/test/resources/uk/gov/tna/dri/schema",$File), "MD5")
        """

      val metaData = """checksum.txt,232762380299115da6995e4c4ac22fa2"""

      validate(metaData, schema) must beLike { case Success(_) => ok }
    }

    "succeed when calculated algorithm does match given cross referenced root & cross referenced string value - with /" in {
      val schema =
        """@totalColumns 3 @noHeader
           Root:
           File:
           MD5: checksum(file($Root,$File), "MD5")
        """

      val metaData = """src/test/resources/uk/gov/tna/dri/schema,checksum.txt,232762380299115da6995e4c4ac22fa2"""

      validate(metaData, schema) must beLike { case Success(_) => ok }
    }
  }
}