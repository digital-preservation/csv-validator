package uk.gov.tna.dri.template

import org.specs2.mutable._

class TemplateReaderSpec extends Specification {

  "The template reader"  should  {

    "check if the template file exists" in {
      TemplateReader.fileReadable("/there/is/no/file/here.txt") must_== false
    }

    "check the template file is readable" in {
      TemplateReader.fileReadable("src/test/resources/template.readable.txt") must_== true
    }
  }

  "totalColumns" should {
    "be present" in {
      TemplateReader.read("src/test/resources/template.with.totalColumns.txt") must_== Some(Template(10))
    }

    "detect not present" in {
      TemplateReader.read("src/test/resources/template.without.totalColumns.txt") must_== None
    }
  }
}
