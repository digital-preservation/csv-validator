package uk.gov.tna.dri.validator

import org.specs2.mutable.Specification
import uk.gov.tna.dri.schema.Schema
import java.io.StringReader
import scalaz._

class MetaDataValidatorSpec extends Specification {


  object TestMetaDataValidator extends MetaDataValidator

  "Validation" should {

    "succeed for correct total columns for multiple lines" in {
      val metaData =
        """col1, col2
           col1, col2"""

      TestMetaDataValidator.validate(new StringReader(metaData), Schema(2)) must beLike { case Success(MetaData(2,_)) => ok }
    }

    "fail for incorrect number of total columns for multiple lines" in {
      val metaData =
        """col1, col2, col3
           col1, col2
           col1, col2, col3"""

      TestMetaDataValidator.validate(new StringReader(metaData), Schema(3)) must beLike { case Failure(f) if f.head ==  "Expected @TotalColumns of 3 and found 2 on line 2" => ok }
    }

    "fail if all columns not valid as per the regex" in {
      val metaData =
        """blah, blah, blah
           blah, blah, blah"""
      TestMetaDataValidator.validate(new StringReader(metaData), Schema(3, Some("blah".r))) must beLike { case Failure(f) if f.head == "fail" => ok}
    }
  }
}
