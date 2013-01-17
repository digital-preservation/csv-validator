package uk.gov.tna.dri.schema

import org.specs2.mutable._
import uk.gov.tna.SchemaReader

class SchemaReaderSpec extends Specification {

  "The schema reader"  should  {

    "check if the file exists" in {
      SchemaReader.fileExists("/there/is/no/file/here.txt") must_== false
    }

  }

}
