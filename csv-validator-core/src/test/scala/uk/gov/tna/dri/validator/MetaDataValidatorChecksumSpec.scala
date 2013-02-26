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


  "Checksum with path/filename in schema" should {
    "succeed when calculated algorithm does match given value" in {
      val schema =
        """version 1.0
           @totalColumns 2 @noHeader
           File:
           MD5: checksum(file("src/test/resources/uk/gov/tna/dri/schema/checksum.txt"),"MD5")
        """

      val metaData = """src/test/resources/uk/gov/tna/dri/schema/checksum.txt,232762380299115da6995e4c4ac22fa2"""

      validate(metaData, schema) must beLike { case Success(_) => ok }
    }

    "fail when calculated algorithm does match not given string value" in {
      val schema =
        """version 1.0
           @totalColumns 2 @noHeader
           File:
           MD5: checksum(file("src/test/resources/uk/gov/tna/dri/schema/checksum.txt"),"MD5")
        """

      val metaData = """src/test/resources/uk/gov/tna/dri/schema/checksum.txt,wrong"""

      validate(metaData, schema) must beLike {
        case Failure(messages) => messages.list mustEqual List("""checksum(file("src/test/resources/uk/gov/tna/dri/schema/checksum.txt")) fails for line: 1, column: MD5, value: wrong""")
      }
    }

  }


  "Checksum with rootpath and filename in schema" should {
    "succeed when calculated algorithm does match given cross referenced string value" in {
      val schema =
        """version 1.0
           @totalColumns 2 @noHeader
           File:
           MD5: checksum(file("src/test/resources/uk/gov/tna/dri/schema","checksum.txt"),"MD5")
        """

      val metaData = """ABC,232762380299115da6995e4c4ac22fa2"""

      validate(metaData, schema) must beLike { case Success(_) => ok }
    }

    "fail when calculated algorithm does match given cross referenced string value" in {
      val schema =
        """version 1.0
           @totalColumns 2 @noHeader
           File:
           MD5: checksum(file("src/test/resources/uk/gov/tna/dri/schema","checksum.txt"),"MD5")
        """

      val metaData = """ABC,wrong"""

      validate(metaData, schema) must beLike {
        case Failure(messages) => messages.list mustEqual List("""checksum(file("src/test/resources/uk/gov/tna/dri/schema", "checksum.txt")) fails for line: 1, column: MD5, value: wrong""")
      }
    }

  }

  "Checksum with root in schema and file in metadata" should {

    "succeed when calculated algorithm does match given root & cross referenced string value" in {
      val schema =
        """version 1.0
           @totalColumns 2 @noHeader
           File:
           MD5: checksum(file("src/test/resources/uk/gov/tna/dri/schema",$File),"MD5")
        """

      val metaData = """checksum.txt,232762380299115da6995e4c4ac22fa2"""

      validate(metaData, schema) must beLike { case Success(_) => ok }
    }

    //    "fail when incorrect root given in schema for root" in {
    //      val schema =
    //        """@totalColumns 2 @noHeader
    //           File:
    //           MD5: checksum(file("invalid/path/to/root",$File), "MD5")
    //        """
    //
    //      val metaData = """checksum.txt,232762380299115da6995e4c4ac22fa2"""
    //
    //      validate(metaData, schema) must beLike {
    //        case Failure(messages) => messages.list mustEqual List("""checksum(file("invalid/path/to/root", $File)) fails for line: 1, column: MD5, value: rubbish""")
    //      }
    //    }


  "Checksum with fullpath in metedata" should {

    "succeed when calculated algorithm does match given cross referenced string value" in {
      val schema =
        """version 1.0
           @totalColumns 2 @noHeader
           File:
           MD5: checksum(file($File),"MD5")
        """

      val metaData = """src/test/resources/uk/gov/tna/dri/schema/checksum.txt,232762380299115da6995e4c4ac22fa2"""

      validate(metaData, schema) must beLike { case Success(_) => ok }
    }

    "fail when calculated algorithm does not match given cross referenced string value" in {
      val schema =
        """version 1.0
           @totalColumns 2 @noHeader
           File:
           MD5: checksum(file($File),"MD5")
        """

      val metaData = """src/test/resources/uk/gov/tna/dri/schema/checksum.txt,rubbish"""

      validate(metaData, schema) must beLike {
        case Failure(messages) => messages.list mustEqual List("checksum(file($File)) fails for line: 1, column: MD5, value: rubbish")
      }
    }
  }

    "fail when calculated algorithm does match" in {
      val schema =
        """version 1.0
           @totalColumns 2 @noHeader
           File:
           MD5: checksum(file("src/test/resources/uk/gov/tna/dri/schema",$File),"MD5")
        """

      val metaData = """checksum.txt,rubbish"""

      validate(metaData, schema) must beLike {
        case Failure(messages) => messages.list mustEqual List("""checksum(file("src/test/resources/uk/gov/tna/dri/schema", $File)) fails for line: 1, column: MD5, value: rubbish""")
      }
    }
  }


  "Checksum with cross reference on both root and file" should {

    "succeed when root and file referance a valid file" in {
      val schema =
        """version 1.0
           @totalColumns 3 @noHeader
           Root:
           File:
           MD5: checksum(file($Root,$File),"MD5")
        """

      val metaData = """src/test/resources/uk/gov/tna/dri/schema,checksum.txt,232762380299115da6995e4c4ac22fa2"""

      validate(metaData, schema) must beLike { case Success(_) => ok }
    }

//    "fail when incorrect root given in metadata" in {
//      val schema =
//        """@totalColumns 3 @noHeader
//           Root:
//           File:
//           MD5: checksum(file($Root,$File), "MD5")
//        """
//
//      val metaData = """invalid/path/to/root,checksum.txt,232762380299115da6995e4c4ac22fa2"""
//
//      validate(metaData, schema) must beLike {
//        case Failure(messages) => messages.list mustEqual List("""checksum(file("src/test/resources/uk/gov/tna/dri/schema", $File)) fails for line: 1, column: MD5, value: rubbish""")
//      }
//    }
  }

  //  "Checksum with multi files matches" should {
  //    "succeed with 3 files" in {
  //      val schema =
  //        """@totalColumns 2 @noHeader
  //           File:
  //           MD5: checksum(file("src/test/resources/uk/gov/tna/dri/schema", $File), "MD5")
  //        """
  //
  //      val metaData = """*.txt,"232762380299115da6995e4c4ac22fa2,232762380299115da6995e4c4ac22fa2,232762380299115da6995e4c4ac22fa2""""
  //
  //      validate(metaData, schema) must beLike { case Success(_) => ok }
  //
  //    }
  //  }


  "Checksum with an algorithm" should {

    "succeed when using a valid algorithm" in {
      val schema =
        """version 1.0
           @totalColumns 2 @noHeader
           File:
           MD5: checksum(file($File),"MD5")
        """

      val metaData = """src/test/resources/uk/gov/tna/dri/schema/checksum.txt,232762380299115da6995e4c4ac22fa2"""

      validate(metaData, schema) must beLike { case Success(_) => ok }
    }

//    "fail when algorithm is invalid/unknown" in {
//      val schema =
//        """@totalColumns 2 @noHeader
//             File:
//             MD5: checksum(file($File), "INVALID")
//        """
//
//      val metaData = """src/test/resources/uk/gov/tna/dri/schema/checksum.txt,232762380299115da6995e4c4ac22fa2"""
//
//      validate(metaData, schema) must beLike {
//        case Failure(messages) => messages.list mustEqual List("checksum(file($File)) fails for line: 1, column: MD5, value: rubbish")
//      }
//    }
  }

}