package uk.gov.tna.dri.validator

import org.specs2.mutable.Specification
import uk.gov.tna.dri.schema.{ColumnDefinition, RegexRule, Schema}
import java.io.StringReader
import scalaz._

class MetaDataValidatorSpec extends Specification {

  object TestMetaDataValidator extends MetaDataValidator

  import TestMetaDataValidator._

  "Validation" should {

    "succeed for correct total columns for multiple lines" in {
      val metaData =
        """col1, col2
           col1, col2"""

      validate(new StringReader(metaData), Schema(2)) must beLike { case Success(_) => ok }
    }

    "fail for incorrect number of total columns for multiple lines" in {
      val metaData =
        """col1, col2, col3
           col1, col2
           col1, col2, col3"""

      validate(new StringReader(metaData), Schema(3)) must beLike { case Failure(messages) => messages.head mustEqual "Expected @TotalColumns of 3 and found 2 on line 2" }
    }

    "fail if value doesnt passes regex rule" in {
      regexForValue("Hello", RegexRule("[0-9]".r)) must beLike { case Failure("Value: Hello does not match regex: [0-9]") => ok }
    }

    "succeed if value passes regex rule" in {
      regexForValue("9", RegexRule("[0-9]".r)) must beLike { case Success(_) => ok }
    }

    "succeed if first column in single row passes regex rule" in {
      regexForRow(List("abcd", "9", "8"), RegexRule("[a-z]*".r)) must beLike { case Success(_) => ok }
    }

    "fail if first column fails regex rule" in {
      regexForRow(List("9", "sdsd", "8"), RegexRule("[a-z]*".r)) must beLike {case Failure("Value: 9 does not match regex: [a-z]*") => ok }
    }

    "succeed if all first column values pass regex rule" in {

      val rows = List (List("23","dfdf","gg4"), List("12","ef","4"), List("1","x","sds"))
      regex(rows, RegexRule("[0-9]*".r)) must beLike { case Success(_) => ok }
    }

    "fail if not all first column values pass regex rule" in {

      val rows = List (List("23","dfdf","gg4"), List("aa","ef","4"), List("qq","x","sds"))
      regex(rows, RegexRule("[0-9]*".r)) must beLike { case Failure(msgs) => msgs.list must contain("Value: aa does not match regex: [0-9]*", "Value: qq does not match regex: [0-9]*")}
    }
    
    "fail if first column doesnt pass regex rule" in {
      val columnDefs = List(ColumnDefinition("first", List(RegexRule("[3-8]*".r))), ColumnDefinition("second"))
      regexForRowWithColumnDef(List("99", "xxx"), columnDefs) must beLike { case Failure(msgs) => msgs.head mustEqual "Value: 99 does not match regex: [3-8]*" }
    }

  }
}
