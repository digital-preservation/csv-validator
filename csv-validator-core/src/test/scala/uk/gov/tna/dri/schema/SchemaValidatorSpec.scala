package uk.gov.tna.dri.schema

import org.specs2.mutable.Specification
import uk.gov.tna.dri.validator.MetaDataValidatorApp
import scalaz.{Failure, Success}
//import org.specs2.execute.Failure

class SchemaValidatorSpec extends Specification {

  "The schema validation " should {

    "succeed for unique column ids" in {

      val schema = Schema(GlobalDirectives(TotalColumnsDirective(2)), ColumnDefinition("Column1") :: ColumnDefinition("Column2") :: Nil)
      val validator = new SchemaValidator{}

      validator.validate(schema) must beLike { case Success(_) => ok}
    }

    "fail for duplicate column ids" in {

      val schema = Schema(GlobalDirectives(TotalColumnsDirective(3)), ColumnDefinition("Name") :: ColumnDefinition("Surname") :: ColumnDefinition("Name") :: Nil)
      val validator = new SchemaValidator{}

      validator.validate(schema) must beLike { case Failure(messages) => messages.list mustEqual List("Column: Name has duplicates in columns 1,3")}
    }


  }

}
