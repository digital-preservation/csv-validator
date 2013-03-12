package uk.gov.tna.dri.schema

import org.specs2.mutable.Specification
import scalaz.{Success, Failure}
import uk.gov.tna.dri.metadata.{Cell, Row}

class ChecksumRuleSpec extends Specification {

  val emptyPathSubstitutions = List[(String,String)]()

  "Checksum" should  {

    "fail when calculated algorithm does not match given string value" in {
      val checksumRule = new ChecksumRule(Literal(Some("build.sbt")), "MD5")

      checksumRule.evaluate(0, Row(List(Cell("699d61aff25f16a5560372e610da91ab")), 1), Schema(List(TotalColumns(1), NoHeader()), List(ColumnDefinition("column1")))) must beLike {
        case Failure(m) => m.list mustEqual List("""checksum(file("build.sbt"), "MD5") checksum match fails for line: 1, column: column1, value: 699d61aff25f16a5560372e610da91ab""")
      }
    }

    "succeed when calculated algorithm does match given string value" in {
      val checksumRule = new ChecksumRule(Literal(Some("""src/test/resources/uk/gov/tna/dri/schema/checksum.txt""")), "MD5")

      checksumRule.evaluate(0, Row(List(Cell("232762380299115da6995e4c4ac22fa2")), 1), Schema(List(TotalColumns(1), NoHeader()), List(ColumnDefinition("column1")))) mustEqual Success(true)
    }

    "succeed when calculated algorithm does match given string value - without /" in {
      val checksumRule = ChecksumRule(Literal(Some("""src/test/resources/uk/gov/tna/dri/schema""")), Literal(Some("""checksum.txt""")), "MD5", emptyPathSubstitutions)

      checksumRule.evaluate(0, Row(List(Cell("232762380299115da6995e4c4ac22fa2")), 1), Schema(List(TotalColumns(1), NoHeader()), List(ColumnDefinition("column1")))) mustEqual Success(true)
    }

    "succeed when calculated algorithm does match given string value - with /" in {
      val checksumRule = ChecksumRule(Literal(Some("""src/test/resources/uk/gov/tna/dri/schema/""")), Literal(Some("""checksum.txt""")), "MD5", emptyPathSubstitutions)

      checksumRule.evaluate(0, Row(List(Cell("232762380299115da6995e4c4ac22fa2")), 1), Schema(List(TotalColumns(1), NoHeader()), List(ColumnDefinition("column1")))) mustEqual Success(true)
    }
  }

  "checksum with path substitutions" should {
    "succeed with help from substitutions to fix path" in {
      val pathSubstitutions =  List[(String,String)](
        ("bob", "src/test")
      )

      val checksumRule = new ChecksumRule(Literal(Some("""bob/resources/uk/gov/tna/dri/schema/checksum.txt""")), "MD5", pathSubstitutions)
      checksumRule.evaluate(0, Row(List(Cell("232762380299115da6995e4c4ac22fa2")), 1), Schema(List(TotalColumns(1), NoHeader()), List(ColumnDefinition("column1")))) mustEqual Success(true)
    }

    "succeed with help from substitutions with extra '/'" in {
      val pathSubstitutions =  List[(String,String)](
        ("bob", "src/test/")
      )

      val checksumRule = new ChecksumRule(Literal(Some("""bob/resources/uk/gov/tna/dri/schema/checksum.txt""")), "MD5", pathSubstitutions)
      checksumRule.evaluate(0, Row(List(Cell("232762380299115da6995e4c4ac22fa2")), 1), Schema(List(TotalColumns(1), NoHeader()), List(ColumnDefinition("column1")))) mustEqual Success(true)
    }

    "succeed with help from substitutions with windows path seperators" in {
      val pathSubstitutions =  List[(String,String)](
        ("bob", "src\\\\test")
      )

      val checksumRule = new ChecksumRule(Literal(Some("bob\\resources\\uk\\gov\\tna\\dri\\schema\\checksum.txt")), "MD5", pathSubstitutions)
      checksumRule.evaluate(0, Row(List(Cell("232762380299115da6995e4c4ac22fa2")), 1), Schema(List(TotalColumns(1), NoHeader()), List(ColumnDefinition("column1")))) mustEqual Success(true)
    }

    "succeed when substitutions is not at the start of the path" in {
      val pathSubstitutions =  List[(String,String)](
        ("bob", "src/test")
      )

      val checksumRule = new ChecksumRule(Literal(Some("""file://bob/resources/uk/gov/tna/dri/schema/checksum.txt""")), "MD5", pathSubstitutions)

      checksumRule.evaluate(0, Row(List(Cell("232762380299115da6995e4c4ac22fa2")), 1), Schema(List(TotalColumns(1), NoHeader()), List(ColumnDefinition("column1")))) must beLike {
        case Failure(m) => m.list mustEqual List("""checksum(file("file://bob/resources/uk/gov/tna/dri/schema/checksum.txt"), "MD5") file "file://src/test/resources/uk/gov/tna/dri/schema/checksum.txt" not found for line: 1, column: column1, value: 232762380299115da6995e4c4ac22fa2""")
      }
    }


  }
}