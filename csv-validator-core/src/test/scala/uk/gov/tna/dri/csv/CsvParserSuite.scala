package uk.gov.tna.dri.csv


import org.scalatest.FunSuite


/**
 * User: Jim Collins
 * Date: 1/16/13
 */
class CsvParserSuite extends CsvParser with FunSuite {


  test("global directive parses NoHeader or IgnoreColumnCase but not both"){
    assert {parseAll(schema, "{ globalDirective{@NoHeader} }") match {
      case Success(_, _) => true
      case _ => false
      }
    }
    assert {parseAll(schema, "{ globalDirective{@IgnoreColumnNameCase } }") match {
      case Success(_, _) => true
      case _ => false
      }
    }
    assert {parseAll(schema, "{ globalDirective{@IgnoreColumnNameCase @NoHeader} }") match {
      case NoSuccess(_, _) => true
      case _ => false
     }
    }
  }

  test("column count is a positive integer") {
    assert {parseAll(schema, "{ globalDirective{@TotalColumns: } }") match {
      case NoSuccess(_, _) => true
      case _ => false
    }
    }
    assert {parseAll(schema, "{ globalDirective{@TotalColumns: -1} }") match {
      case NoSuccess(_, _) => true
      case _ => false
    }
    }
    assert {parseAll(schema, "{ globalDirective{@TotalColumns: 1} }") match {
      case Success(_, _) => true
      case _ => false
    }
    }
    assert {parseAll(schema, "{ globalDirective{@TotalColumns:    100000    } }") match {
      case Success(_, _) => true
      case _ => false
    }
    }
  }

}