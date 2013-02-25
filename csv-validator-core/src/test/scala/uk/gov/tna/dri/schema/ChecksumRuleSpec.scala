package uk.gov.tna.dri.schema

import org.specs2.mutable.Specification
import scalaz.{Success, Failure}
import uk.gov.tna.dri.metadata.{Cell, Row}

class ChecksumRuleSpec extends Specification {

  "Checksum" should  {

    "fail when calculated algorithm does not match given string value" in {
      val checksumRule = ChecksumRule(Literal(Some("build.sbt")), "MD5")

      checksumRule.evaluate(0, Row(List(Cell("699d61aff25f16a5560372e610da91ab")), 1), Schema(List(TotalColumns(1), NoHeader()), List(ColumnDefinition("column1")))) must beLike {
        case Failure(m) => m.list mustEqual List("""checksum("build.sbt") fails for line: 1, column: column1, value: 699d61aff25f16a5560372e610da91ab""")
      }
    }

    "succeed when calculated algorithm does match given string value" in {
      val checksumRule = ChecksumRule(Literal(Some("""src/test/resources/uk/gov/tna/dri/schema/checksum.txt""")), "MD5")

      checksumRule.evaluate(0, Row(List(Cell("232762380299115da6995e4c4ac22fa2")), 1), Schema(List(TotalColumns(1), NoHeader()), List(ColumnDefinition("column1")))) mustEqual Success(true)
    }
  }
}