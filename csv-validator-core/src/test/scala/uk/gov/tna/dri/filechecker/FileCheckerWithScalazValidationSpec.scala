package uk.gov.tna.dri.filechecker

import org.specs2.mutable.Specification
import scalaz._

class FileCheckerWithScalazValidationSpec extends Specification {


  "file checker" should {

    "fail with message when there is an unreadable file" in {

      FileCheckerWithScalazValidation.check("src/test/resources/input/someMissingFile.txt, text") match {

        case Success(_) => failure("Should have got a failure message")
        case Failure(failureMessages) => failureMessages.head must_== "Could not read: src/test/resources/input/someMissingFile.txt"
      }

    }

    "succeed when the file is readable" in {
      FileCheckerWithScalazValidation.check("src/test/resources/input/recordsFile1.txt, text") match {

        case Failure(_) => failure("Should have been successful")
        case Success(DataFile(filePath, _)) => filePath must_== "src/test/resources/input/recordsFile1.txt"
      }
    }


    "fail with message when the file type is invalid" in {

      FileCheckerWithScalazValidation.check("src/test/resources/input/recordsFile1.txt, tixt") match {

        case Success(_) => failure("Should have got a failure message")
        case Failure(failureMessages) => failureMessages.head must_== "Invalid file type: tixt"
      }
    }

    "fail with two messages when the file is unreadable and the file type is invalid" in {

         FileCheckerWithScalazValidation.check("src/test/resources/input/anotherMissingFile.txt, data") match {

           case Success(_) => failure("Should have got a failure message")
           case Failure(failureMessages) => failureMessages.list must_== List("Could not read: src/test/resources/input/anotherMissingFile.txt", "Invalid file type: data")
         }
       }

  }
}
