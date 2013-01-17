package uk.gov.tna.dri.schema

import org.specs2.mutable._

class SchemaReaderSpec extends Specification {

  "The schema reader"  should  {

    "check if the file exists" in {
      SchemaReader.fileReadable("/there/is/no/file/here.txt") must_== false
    }

    "check the file is readable" in {
      SchemaReader.fileReadable("src/test/resources/schema.readable.txt") must_== true
    }

  }

}
