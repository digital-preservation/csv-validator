package uk.gov.tna.dri.schema

import org.specs2.mutable.Specification
import scalaz._
import Scalaz._
import net.liftweb.json.scalaz.JsonScalaz._
import net.liftweb.json._


class JSONSpec extends Specification {

  implicit val formats = DefaultFormats

  case class Schema(separator: Option[String], totalColumns: Int, noHeaderDirective: Boolean)


  "JSON parsing with Lift" should {

    "allow missing fields to be None" in {

      parse(""" {"totalColumns":5,"noHeaderDirective":true} """).extract[Schema] must_== Schema(None, 5, true)
    }

  }

  "JSON parsing with lift and scalaz" should {

    "allow us to validate using the nice scalaz validators" in {
      import Validation.Monad._

      def min(x: Int): Int => Result[Int] = (y: Int) =>
        if (y < x) Fail("min", y + " < " + x) else y.success


      val json = JsonParser.parse( """ {"separator":",","totalColumns":-5,"noHeaderDirective":true} """)

      val schema = Schema.applyJSON(field("separator"), validate[Int]("totalColumns") >=> min(0), field("noHeaderDirective"))(json)

      schema match {
        case Failure(errors) => errors.head must_== UncategorizedError("min", "-5 < 0", Nil)
        case _ => failure("Expected a validation error on totalColumns")
      }
    }
  }

}
