package uk.gov.tna.dri.validator

import org.specs2.mutable.Specification
import scalaz._
import uk.gov.tna.dri.schema.Schema

class MetaDataValidatorBigFileSpec extends Specification {

  val basePath = "src/test/resources/uk/gov/tna/dri/validator/acceptance/"

  "Big file" should {

    "succeed with no stack overflow for all errors" in {
      val v = new MetaDataValidatorApp with AllErrorsMetaDataValidator { val pathSubstitutions = List[(String,String)]() }
      def parse(filePath: String): Schema = v.parseSchema(filePath) fold (f => throw new IllegalArgumentException(f.toString), s => s)

      v.validate(basePath + "bigMetaData.csv", parse(basePath + "bigSchema.txt")) must beLike { case Success(_) => ok }
    }

    "succeed with no stack overflow for fail fast" in {
      val v = new MetaDataValidatorApp with FailFastMetaDataValidator { val pathSubstitutions = List[(String,String)]() }
      def parse(filePath: String): Schema = v.parseSchema(filePath) fold (f => throw new IllegalArgumentException(f.toString), s => s)

      v.validate(basePath + "bigMetaData.csv", parse(basePath + "bigSchema.txt")) must beLike { case Success(_) => ok }
    }
  }
}