/**
 * Copyright (c) 2013, The National Archives <digitalpreservation@nationalarchives.gov.uk>
 * http://www.nationalarchives.gov.uk
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package uk.gov.nationalarchives.csv.validator.schema.v1_1

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import uk.gov.nationalarchives.csv.validator.metadata.{Cell, Row}
import uk.gov.nationalarchives.csv.validator.schema.{ColumnDefinition, NamedColumnIdentifier, Schema, TotalColumns}

import scalaz.{Failure, Success}

@RunWith(classOf[JUnitRunner])
class CaseRuleSpec extends Specification {

  "UpperCaseRule" should {
    "succeed when row contain upper case Letters" in {
      val globalDirectives = List(TotalColumns(1))
      val schema = Schema(globalDirectives, List(ColumnDefinition(NamedColumnIdentifier("Country"))))
      
      val upperCaseRule = UpperCaseRule()
      upperCaseRule.evaluate(0, Row(List(Cell("GERMANY")), 1), schema) mustEqual Success(true)
    }

    "succeed when row contain upper case Letters - non English Alphabet" in {
      val globalDirectives = List(TotalColumns(1))
      val schema = Schema(globalDirectives, List(ColumnDefinition(NamedColumnIdentifier("Country"))))

      val upperCaseRule = UpperCaseRule()
      upperCaseRule.evaluate(0, Row(List(Cell("ΑΡΣΕΝΑΛ")), 1), schema) mustEqual Success(true)
    }



    "succeed when row contain upper case letter and white space" in {
      val globalDirectives = List(TotalColumns(1))
      val schema = Schema(globalDirectives, List(ColumnDefinition(NamedColumnIdentifier("Country"))))


      val upperCaseRule = UpperCaseRule()

      upperCaseRule.evaluate(0, Row(List(Cell("UNITED KINGDOM")), 1), schema) mustEqual Success(true)
    }

    "succeed when row contain upper case letter white space " in {
      val globalDirectives = List(TotalColumns(1))
      val schema = Schema(globalDirectives, List(ColumnDefinition(NamedColumnIdentifier("Country"))))


      val upperCaseRule = UpperCaseRule()

      upperCaseRule.evaluate(0, Row(List(Cell("UNITED KINGDOM")), 1), schema) mustEqual Success(true)
    }

    "succeed when row contain upper case letter white space and numbers" in {
      val globalDirectives = List(TotalColumns(1))
      val schema = Schema(globalDirectives, List(ColumnDefinition(NamedColumnIdentifier("Country"))))


      val upperCaseRule = UpperCaseRule()

      upperCaseRule.evaluate(0, Row(List(Cell("UNITED KINGDOM 1234569")), 1), schema) mustEqual Success(true)
    }

    "succeed when row contain upper case letter, white space ,numbers and punctuations" in {
      val globalDirectives = List(TotalColumns(1))
      val schema = Schema(globalDirectives, List(ColumnDefinition(NamedColumnIdentifier("Country"))))


      val upperCaseRule = UpperCaseRule()

      upperCaseRule.evaluate(0, Row(List(Cell("UNITED KINGDOM 1234569 ???!!!%%")), 1), schema) mustEqual Success(true)
    }

    "fail when row contain lower case letters" in {
      val globalDirectives = List(TotalColumns(1))
      val schema = Schema(globalDirectives, List(ColumnDefinition(NamedColumnIdentifier("Country"))))


      val upperCaseRule = UpperCaseRule()


      upperCaseRule.evaluate(0, Row(List(Cell("germany")), 1), schema) must beLike {
        case Failure(messages) => messages.list mustEqual List("upperCase fails for line: 1, column: Country, value: \"germany\"")
      }

    }
    

    "fail when row contain upper case Letters and lower case" in {
      val globalDirectives = List(TotalColumns(1))
      val schema = Schema(globalDirectives, List(ColumnDefinition(NamedColumnIdentifier("Country"))))


      val upperCaseRule = UpperCaseRule()


      upperCaseRule.evaluate(0, Row(List(Cell("GeRMANY")), 1), schema) must beLike {
        case Failure(messages) => messages.list mustEqual List("upperCase fails for line: 1, column: Country, value: \"GeRMANY\"")
      }
      upperCaseRule.evaluate(0, Row(List(Cell("GeRmANy")), 2), schema) must beLike {
        case Failure(messages) => messages.list mustEqual List("upperCase fails for line: 2, column: Country, value: \"GeRmANy\"")
      }
      upperCaseRule.evaluate(0, Row(List(Cell("gERMANY")), 3), schema) must beLike {
        case Failure(messages) => messages.list mustEqual List("upperCase fails for line: 3, column: Country, value: \"gERMANY\"")
      }

    }

    "fail when row contain upper case Letters and lower case - non English Alphabet" in {
      val globalDirectives = List(TotalColumns(1))
      val schema = Schema(globalDirectives, List(ColumnDefinition(NamedColumnIdentifier("Country"))))


      val upperCaseRule = UpperCaseRule()


      upperCaseRule.evaluate(0, Row(List(Cell("ΑρΣΕΝΑΛ")), 1), schema) must beLike {
        case Failure(messages) => messages.list mustEqual List("upperCase fails for line: 1, column: Country, value: \"ΑρΣΕΝΑΛ\"")
      }
      upperCaseRule.evaluate(0, Row(List(Cell("ΑρΣεΝΑΛ")), 2), schema) must beLike {
        case Failure(messages) => messages.list mustEqual List("upperCase fails for line: 2, column: Country, value: \"ΑρΣεΝΑΛ\"")
      }
      upperCaseRule.evaluate(0, Row(List(Cell("αΡΣΕΝΑΛ")), 3), schema) must beLike {
        case Failure(messages) => messages.list mustEqual List("upperCase fails for line: 3, column: Country, value: \"αΡΣΕΝΑΛ\"")
      }

    }


    "fail when row contain upper case Letters, lower case, and whiteSpace" in {
      val globalDirectives = List(TotalColumns(1))
      val schema = Schema(globalDirectives, List(ColumnDefinition(NamedColumnIdentifier("Country"))))


      val upperCaseRule = UpperCaseRule()

      upperCaseRule.evaluate(0, Row(List(Cell("United Kingdom")), 1), schema) must beLike {
        case Failure(messages) => messages.list mustEqual List("upperCase fails for line: 1, column: Country, value: \"United Kingdom\"")
      }
      upperCaseRule.evaluate(0, Row(List(Cell("uNITED KINGDOM")), 2), schema) must beLike {
        case Failure(messages) => messages.list mustEqual List("upperCase fails for line: 2, column: Country, value: \"uNITED KINGDOM\"")
      }
    }

    "fail when row contain upper succeed when row contain upper case letter, lowercase letter white space and numbers" in {
      val globalDirectives = List(TotalColumns(1))
      val schema = Schema(globalDirectives, List(ColumnDefinition(NamedColumnIdentifier("Country"))))


      val upperCaseRule = UpperCaseRule()

      upperCaseRule.evaluate(0, Row(List(Cell("United Kingdom 11111")), 1), schema) must beLike {
        case Failure(messages) => messages.list mustEqual List("upperCase fails for line: 1, column: Country, value: \"United Kingdom 11111\"")
      }
      upperCaseRule.evaluate(0, Row(List(Cell("uNITED KINGDOM 12345")), 2), schema) must beLike {
        case Failure(messages) => messages.list mustEqual List("upperCase fails for line: 2, column: Country, value: \"uNITED KINGDOM 12345\"")
      }
    }

    "fail when row contain upper case letter, lower case letter, white space ,numbers and punctuations" in {
      val globalDirectives = List(TotalColumns(1))
      val schema = Schema(globalDirectives, List(ColumnDefinition(NamedColumnIdentifier("Country"))))


      val upperCaseRule = UpperCaseRule()

      upperCaseRule.evaluate(0, Row(List(Cell("United Kingdom 11111 ??")), 1), schema) must beLike {
        case Failure(messages) => messages.list mustEqual List("upperCase fails for line: 1, column: Country, value: \"United Kingdom 11111 ??\"")
      }
      upperCaseRule.evaluate(0, Row(List(Cell("uNITED KINGDOM 12345 ?!\"£")), 2), schema) must beLike {
        case Failure(messages) => messages.list mustEqual List("upperCase fails for line: 2, column: Country, value: \"uNITED KINGDOM 12345 ?!\"£\"")
      }
    }
    
  }

  "LowerCaseRule" should {
    "succed when row contain lower case Letters" in {
      val globalDirectives = List(TotalColumns(1))
      val schema = Schema(globalDirectives, List(ColumnDefinition(NamedColumnIdentifier("Country"))))

      val lowerCaseRule = LowerCaseRule()

      lowerCaseRule.evaluate(0, Row(List(Cell("germany")), 1), schema) mustEqual Success(true)
    }

    "succed when row contain lower case Letters -non english alphabet" in {
      val globalDirectives = List(TotalColumns(1))
      val schema = Schema(globalDirectives, List(ColumnDefinition(NamedColumnIdentifier("Country"))))

      val lowerCaseRule = LowerCaseRule()

      lowerCaseRule.evaluate(0, Row(List(Cell("ωελξ")), 1), schema) mustEqual Success(true)
    }




    "fail when row contain upper case  and lower case letters" in {
      val globalDirectives = List(TotalColumns(1))
      val schema = Schema(globalDirectives, List(ColumnDefinition(NamedColumnIdentifier("Country"))))

      val lowerCaseRule = LowerCaseRule()
      
      lowerCaseRule.evaluate(0, Row(List(Cell("GERMANY")), 1), schema) must beLike {
        case Failure(messages) => messages.list mustEqual List("lowerCase fails for line: 1, column: Country, value: \"GERMANY\"")
      }

      lowerCaseRule.evaluate(0, Row(List(Cell("GeRMaNy")), 2), schema) must beLike {
        case Failure(messages) => messages.list mustEqual List("lowerCase fails for line: 2, column: Country, value: \"GeRMaNy\"")
      }

      lowerCaseRule.evaluate(0, Row(List(Cell("Germany")), 3), schema) must beLike {
        case Failure(messages) => messages.list mustEqual List("lowerCase fails for line: 3, column: Country, value: \"Germany\"")
      }
    }

    "fail when row contain upper case  and lower case letters - non English alphabet" in {
      val globalDirectives = List(TotalColumns(1))
      val schema = Schema(globalDirectives, List(ColumnDefinition(NamedColumnIdentifier("Country"))))

      val lowerCaseRule = LowerCaseRule()

      lowerCaseRule.evaluate(0, Row(List(Cell("ΑρΣΕΝΑΛ")), 1), schema) must beLike {
        case Failure(messages) => messages.list mustEqual List("lowerCase fails for line: 1, column: Country, value: \"ΑρΣΕΝΑΛ\"")
      }
      lowerCaseRule.evaluate(0, Row(List(Cell("ΑρΣεΝΑΛ")), 2), schema) must beLike {
        case Failure(messages) => messages.list mustEqual List("lowerCase fails for line: 2, column: Country, value: \"ΑρΣεΝΑΛ\"")
      }
      lowerCaseRule.evaluate(0, Row(List(Cell("αΡΣΕΝΑΛ")), 3), schema) must beLike {
        case Failure(messages) => messages.list mustEqual List("lowerCase fails for line: 3, column: Country, value: \"αΡΣΕΝΑΛ\"")
      }
    }



    "succeed when row contain lower case letter and white space" in {
      val globalDirectives = List(TotalColumns(1))
      val schema = Schema(globalDirectives, List(ColumnDefinition(NamedColumnIdentifier("Country"))))

      val lowerCaseRule = LowerCaseRule()
      lowerCaseRule.evaluate(0, Row(List(Cell("united kingdom")), 1), schema) mustEqual Success(true)
    }

    "fail when row contain lower case letter and white space" in {
      val globalDirectives = List(TotalColumns(1))
      val schema = Schema(globalDirectives, List(ColumnDefinition(NamedColumnIdentifier("Country"))))

      val lowerCaseRule = LowerCaseRule()
      lowerCaseRule.evaluate(0, Row(List(Cell("United Kingdom")), 1), schema) must beLike {
        case Failure(messages) => messages.list mustEqual List("lowerCase fails for line: 1, column: Country, value: \"United Kingdom\"")
      }
    }



    "succeed when row contain lower case letter white space and numbers" in {
      val globalDirectives = List(TotalColumns(1))
      val schema = Schema(globalDirectives, List(ColumnDefinition(NamedColumnIdentifier("Country"))))


      val lowerCaseRule = LowerCaseRule()
      lowerCaseRule.evaluate(0, Row(List(Cell("united kingdom 1234569")), 1), schema) mustEqual Success(true)
    }

    "fail when row contain lower case and upper case letter white space and numbers" in {
      val globalDirectives = List(TotalColumns(1))
      val schema = Schema(globalDirectives, List(ColumnDefinition(NamedColumnIdentifier("Country"))))


      val lowerCaseRule = LowerCaseRule()
      lowerCaseRule.evaluate(0, Row(List(Cell("united Kingdom 1234569")), 1), schema)must beLike {
        case Failure(messages) => messages.list mustEqual List("lowerCase fails for line: 1, column: Country, value: \"united Kingdom 1234569\"")
      }
    }

    "succeed when row contain lower case letter, white space ,numbers and punctuations" in {
      val globalDirectives = List(TotalColumns(1))
      val schema = Schema(globalDirectives, List(ColumnDefinition(NamedColumnIdentifier("Country"))))


      val lowerCaseRule = LowerCaseRule()

      lowerCaseRule.evaluate(0, Row(List(Cell("united kingdom 1234569 ???!!!%%")), 1), schema) mustEqual Success(true)
    }

    "fail when row contain lower case letters" in {
      val globalDirectives = List(TotalColumns(1))
      val schema = Schema(globalDirectives, List(ColumnDefinition(NamedColumnIdentifier("Country"))))


      val upperCaseRule = UpperCaseRule()


      upperCaseRule.evaluate(0, Row(List(Cell("germany")), 1), schema) must beLike {
        case Failure(messages) => messages.list mustEqual List("upperCase fails for line: 1, column: Country, value: \"germany\"")
      }

    }


    "fail when row contain upper case Letters and lower case" in {
      val globalDirectives = List(TotalColumns(1))
      val schema = Schema(globalDirectives, List(ColumnDefinition(NamedColumnIdentifier("Country"))))


      val upperCaseRule = UpperCaseRule()


      upperCaseRule.evaluate(0, Row(List(Cell("GeRMANY")), 1), schema) must beLike {
        case Failure(messages) => messages.list mustEqual List("upperCase fails for line: 1, column: Country, value: \"GeRMANY\"")
      }
      upperCaseRule.evaluate(0, Row(List(Cell("GeRmANy")), 2), schema) must beLike {
        case Failure(messages) => messages.list mustEqual List("upperCase fails for line: 2, column: Country, value: \"GeRmANy\"")
      }
      upperCaseRule.evaluate(0, Row(List(Cell("gERMANY")), 3), schema) must beLike {
        case Failure(messages) => messages.list mustEqual List("upperCase fails for line: 3, column: Country, value: \"gERMANY\"")
      }

    }

    "fail when row contain upper case Letters, lower case, and whiteSpace" in {
      val globalDirectives = List(TotalColumns(1))
      val schema = Schema(globalDirectives, List(ColumnDefinition(NamedColumnIdentifier("Country"))))


      val upperCaseRule = UpperCaseRule()

      upperCaseRule.evaluate(0, Row(List(Cell("United Kingdom")), 1), schema) must beLike {
        case Failure(messages) => messages.list mustEqual List("upperCase fails for line: 1, column: Country, value: \"United Kingdom\"")
      }
      upperCaseRule.evaluate(0, Row(List(Cell("uNITED KINGDOM")), 2), schema) must beLike {
        case Failure(messages) => messages.list mustEqual List("upperCase fails for line: 2, column: Country, value: \"uNITED KINGDOM\"")
      }
    }

    "fail when row contain upper succeed when row contain upper case letter, lowercase letter white space and numbers" in {
      val globalDirectives = List(TotalColumns(1))
      val schema = Schema(globalDirectives, List(ColumnDefinition(NamedColumnIdentifier("Country"))))


      val upperCaseRule = UpperCaseRule()

      upperCaseRule.evaluate(0, Row(List(Cell("United Kingdom 11111")), 1), schema) must beLike {
        case Failure(messages) => messages.list mustEqual List("upperCase fails for line: 1, column: Country, value: \"United Kingdom 11111\"")
      }
      upperCaseRule.evaluate(0, Row(List(Cell("uNITED KINGDOM 12345")), 2), schema) must beLike {
        case Failure(messages) => messages.list mustEqual List("upperCase fails for line: 2, column: Country, value: \"uNITED KINGDOM 12345\"")
      }
    }

    "fail when row contain upper case letter, lower case letter, white space ,numbers and punctuations" in {
      val globalDirectives = List(TotalColumns(1))
      val schema = Schema(globalDirectives, List(ColumnDefinition(NamedColumnIdentifier("Country"))))


      val upperCaseRule = UpperCaseRule()

      upperCaseRule.evaluate(0, Row(List(Cell("United Kingdom 11111 ??")), 1), schema) must beLike {
        case Failure(messages) => messages.list mustEqual List("upperCase fails for line: 1, column: Country, value: \"United Kingdom 11111 ??\"")
      }
      upperCaseRule.evaluate(0, Row(List(Cell("uNITED KINGDOM 12345 ?!\"£")), 2), schema) must beLike {
        case Failure(messages) => messages.list mustEqual List("upperCase fails for line: 2, column: Country, value: \"uNITED KINGDOM 12345 ?!\"£\"")
      }
    }

  }
}