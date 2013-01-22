package uk.gov.tna.dri.filechecker

import org.specs2.mutable.Specification

class FileCheckerSpec extends Specification {


  "file checker" should {

    "succeed for a valid readable file" in {

      FileChecker.check("src/test/resources/input/metaData.csv") must_== true
    }

    "fail when there is an unreadable file" in {

      FileChecker.check("src/test/resources/input/metaDataWithUnreadableFiles.csv") must_== false
    }

  }
}
