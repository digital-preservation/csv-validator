package uk.gov.tna.dri.validator

import org.specs2.mutable.Specification
import scalaz.{Failure, Success}

class SimpleValidatorSpec extends Specification {

  val validator = new SimpleValidator{}

  "Validator" should {

    "succeed for int" in {
      validator.int("2") must beLike {case Success(_) => ok }
    }

    "fail for non int" in {
      validator.int("a") must beLike {case Failure(msg) if msg.head == "Invalid int" => ok}
    }

    "succeed for string length 2" in {
      validator.strLengthTwo("ab") must beLike {case Success(_) => ok}
    }

    "success for two digit int" in {
      validator.intLengthTwo("23") must beLike {case Success(_) => ok}
    }

    "fail for 2 digit string not int" in {
      validator.intLengthTwo("2a") must beLike {case Failure(msg) if msg.head == "Invalid int" => ok}
    }

   "fail for str not length 2 and not an int" in {
     validator.intLengthTwo("2a3") must beLike {case Failure(msg) => msg.list must contain("Invalid int", "String length not 2").only}
   }

  }
}
