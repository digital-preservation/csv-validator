package uk.gov.tna.dri.schema

import org.specs2.mutable._
import java.io.StringReader

import scalaz.{Success => SuccessZ, Failure => FailureZ}

class SchemaParserRulesSpec extends Specification {

  val emptyPathSubstitutions = List[(String,String)]()

  object TestSchemaParser extends SchemaParser { val pathSubstitutions = List[(String,String)]() }

  import TestSchemaParser._

  "Schema" should {

    "succeed for valid regex rule" in {
      val schema = """version 1.0
                      @totalColumns 1
                      LastName: regex("[a]")"""

      parse(new StringReader(schema)) must beLike { case Success(Schema(_, List(ColumnDefinition("LastName", List(RegexRule(r)), _))), _) => r mustEqual "[a]" }
    }

    "fail for an invalid regex" in {
      val schema = """version 1.0
                      @totalColumns 1
                      Something: regex("[0-9")"""

      parseAndValidate(new StringReader(schema)) must beLike {
        case FailureZ(msgs) => msgs.list mustEqual List("Column: Something: Invalid regex(\"[0-9\"): at line: 3, column: 34")
      }
    }

    "fail for missing quotes defining a regex" in {
      val schema = """version 1.0
                      @totalColumns 3
                      LastName:
                      FirstName: regex("a)
                      Age:"""

      parse(new StringReader(schema)) must beLike {
        case Failure(message, _) => message mustEqual """regex not correctly delimited as ("your regex")"""
      }
    }

    "fail for missing value in regex" in {
      val schema = """version 1.0
                      @totalColumns 1
                      Something: regex"""

      parse(new StringReader(schema)) must beLike { case Failure(message, _) => (message mustEqual "regex not correctly delimited as (\"your regex\")") }
    }

    "succeed for more than 1 regex rule" in {
      val schema = """version 1.0
                      @totalColumns 1
                      LastName: regex("[a]") regex("[0-5]")"""

      parse(new StringReader(schema)) must beLike { case Success(_, _) => ok }
    }

    "succeed for file exists rule" in {
      val schema = """version 1.0
                      @totalColumns 1
                      Name: fileExists"""

      parse(new StringReader(schema)) must beLike { case Success(Schema(_, List(ColumnDefinition("Name", List(FileExistsRule(emptyPathSubs, Literal(None))), _))), _) => ok }
    }

//    "fail for file exists rule with empty ()" in {
//      val schema = """version 1.0
//                      @totalColumns 1
//                      Name: fileExists()"""
//
//      parse(new StringReader(schema)) must beLike { case Failure("fileExists rule has an invalid file path", _) => ok }
//    }

    "succeed for file exists rule with root file path" in {
      val schema = """version 1.0
                      @totalColumns 1
                      Name: fileExists("some/root/path")"""

      parse(new StringReader(schema)) must beLike {
        case Success(Schema(_, List(ColumnDefinition("Name", List(FileExistsRule(emptyPathSubs, Literal(Some(rootPath)))), _))), _) => {
          rootPath mustEqual "some/root/path"
        }
      }
    }

//    "fail for non quoted root file path" in {
//      val schema = """version 1.0
//                      @totalColumns 1
//                      Name: fileExists(some/other/root/path)"""
//
//      parse(new StringReader(schema)) must beLike { case Failure("fileExists rule has an invalid file path", _) => ok }
//    }

    "fail for non parentheses" in {
      val schema = """version 1.0
                      @totalColumns 1
                      Name: fileExists /root/path"""

      parse(new StringReader(schema)) must beLike { case Failure("Invalid schema text", _) => ok }
    }

    "succeed for or rule" in {
      val schema =
        """version 1.0
           @totalColumns 1
           Country: in("UK") or in("England")"""

      parse(new StringReader(schema)) must beLike {
        case Success(Schema(_, List(ColumnDefinition("Country", List( OrRule( InRule(Literal(Some("UK"))), InRule(Literal(Some("England"))))), _))), _) => ok
      }
    }

    "fail for or rule with no lhs" in {
      val schema =
        """version 1.0
           @totalColumns 1
           Country: or in("England")"""

      parse(new StringReader(schema)) must beLike { case Failure("Invalid schema text", _) => ok }
    }

    "fail for or rule with no rhs" in {
      val schema =
        """version 1.0
           @totalColumns 1
           Country: in("UK") or"""

      parse(new StringReader(schema)) must beLike { case Failure("Invalid rule", _) => ok }
    }

    "fail for or rule with no lhs or rhs" in {
      val schema =
        """version 1.0
           @totalColumns 1
           Country: or"""

      parse(new StringReader(schema)) must beLike { case Failure("Invalid schema text", _) => ok }
    }

    "succeed for two 'or' rules" in {
      val schema =
        """version 1.0
           @totalColumns 1
           Country: in("UK") or in("England") or in("France")"""

      parse(new StringReader(schema)) must beLike {
        case Success(Schema(_, List(ColumnDefinition("Country",
                               List(OrRule(InRule(Literal(Some("UK"))), OrRule(InRule(Literal(Some("England"))), InRule(Literal(Some("France")))))  ), _))), _) => ok
      }
    }

    "succeed for all no argument rules with hard coded expressions" in {
      val schema =
        """version 1.0
           @totalColumns 1
           Country: uri xDateTime xDate ukDate xTime uuid4 positiveInteger"""

      parse(new StringReader(schema)) must beLike {
        case Success(Schema(_, List(ColumnDefinition("Country", List( UriRule(),XsdDateTimeRule(),XsdDateRule(),UkDateRule(),XsdTimeRule(),Uuid4Rule(),PositiveIntegerRule()), _))), _) => ok
      }
    }

    "succeed for unique rule" in {
      val schema =
        """version 1.0
           @totalColumns 1
           Country: unique"""

      parse(new StringReader(schema)) must beLike {
        case Success(Schema(_, List(ColumnDefinition("Country", List(UniqueRule()), _))), _) => ok
      }
    }

    "succeed for checksum with supported algorithm" in {
      val schema =
        """version 1.0
           @totalColumns 1
           FileChecksum: checksum(file("build.sbt"), "MD5")"""

      parse(new StringReader(schema)) must beLike {
        case Success(Schema(_, _), _) => ok
      }
    }

    "fail when algorithm is invalid/unknown" in {
      val schema =
        """version 1.0
           @totalColumns 2 @noHeader
           File:
           MD5: checksum(file($File), "INVALID")
        """

      parseAndValidate(new StringReader(schema)) must beLike {
        case FailureZ(msgs) => msgs.list mustEqual List("""Column: MD5: Invalid Algorithm: 'INVALID' at line: 4, column: 17""")
      }
    }

    "fail when 2 columns have invalid algorithms" in {
      val schema =
        """version 1.0
           @totalColumns 3 @noHeader
           File:
           MD5: checksum(file($File), "INVALID")
           chksum: checksum(file($File), "WRONG")
        """

      parseAndValidate(new StringReader(schema)) must beLike {
        case FailureZ(msgs) => msgs.list mustEqual List("Column: MD5: Invalid Algorithm: 'INVALID' at line: 4, column: 17\nColumn: chksum: Invalid Algorithm: 'WRONG' at line: 5, column: 20")
      }
    }

    "fail when having one valid and one invalid algorithm" in {
      val schema =
        """version 1.0
           @totalColumns 3 @noHeader
           File:
           MD5: checksum(file($File), "INVALID")
           chksum: checksum(file($File), "MD5")
        """

      parseAndValidate(new StringReader(schema)) must beLike {
        case FailureZ(msgs) => msgs.list mustEqual List("""Column: MD5: Invalid Algorithm: 'INVALID' at line: 4, column: 17""")
      }
    }

  }

  "succeed for xDateTime range with valid date" in {
    val schema =
      """version 1.0
           @totalColumns 1
           Country: xDateTime( 2012-12-01T01:01:01 , 2012-12-01T01:01:01 )"""

    parseAndValidate(new StringReader(schema)) must beLike {
        case SuccessZ(Schema(_, _))  => ok
    }
  }

  "fail for xDateTime range with invalid date" in {
    val schema =
      """version 1.0
           @totalColumns 1
           Country: xDateTime( 2012-99-01T01:01:01 , 2012-12-01T01:01:01 )"""

    parseAndValidate(new StringReader(schema)) must beLike { case FailureZ(msgs) => msgs.list mustEqual List("Column: Country: Invalid xDateTime(\"2012-99-01T01:01:01, 2012-12-01T01:01:01\"): at line: 3, column: 21") }
  }

  "fail for xDateTime range with from > to" in {
    val schema =
      """version 1.0
           @totalColumns 1
           Country: xDateTime( 2012-01-02T01:01:02 , 2012-01-01T01:01:02 )"""

    parseAndValidate(new StringReader(schema)) must beLike { case FailureZ(msgs) => msgs.list mustEqual List("Column: Country: Invalid xDateTime(\"2012-01-02T01:01:02, 2012-01-01T01:01:02\"): at line: 3, column: 21") }
  }


  "succeed for xDate range with valid date" in {
    val schema =
      """version 1.0
           @totalColumns 1
           Country: xDate( 2012-12-01 , 2012-12-01 )"""

    parseAndValidate(new StringReader(schema)) must beLike {
      case SuccessZ(Schema(_, _))  => ok
    }
  }

  "fail for xDate range with invalid date" in {
    val schema =
      """version 1.0
           @totalColumns 1
           Country: xDate( 2012-99-01 , 2012-12-01 )"""

    parseAndValidate(new StringReader(schema)) must beLike { case FailureZ(msgs) => msgs.list mustEqual List("Column: Country: Invalid xDate(\"2012-99-01, 2012-12-01\"): at line: 3, column: 21") }
  }

  "fail for xDate range with from > to" in {
    val schema =
      """version 1.0
           @totalColumns 1
           Country: xDate( 2012-01-02 , 2012-01-01 )"""

    parseAndValidate(new StringReader(schema)) must beLike { case FailureZ(msgs) => msgs.list mustEqual List("Column: Country: Invalid xDate(\"2012-01-02, 2012-01-01\"): at line: 3, column: 21") }
  }

  "succeed for ukDate range with valid date" in {
    val schema =
      """version 1.0
           @totalColumns 1
           Country: ukDate( 01/02/1966 , 01/02/1966 )"""

    parseAndValidate(new StringReader(schema)) must beLike {
      case SuccessZ(Schema(_, _))  => ok
    }
  }

  "fail for ukDate range with invalid date" in {
    val schema =
      """version 1.0
           @totalColumns 1
           Country: ukDate( 30/02/1900 , 30/02/1902 )"""

    parseAndValidate(new StringReader(schema)) must beLike { case FailureZ(msgs) => msgs.list mustEqual List("Column: Country: Invalid ukDate(\"30/02/1900, 30/02/1902\"): at line: 3, column: 21") }
  }

  "fail for ukDate range with from > to" in {
    val schema =
      """version 1.0
           @totalColumns 1
           Country: ukDate( 02/02/1966 , 01/02/1966 )"""

    parseAndValidate(new StringReader(schema)) must beLike { case FailureZ(msgs) => msgs.list mustEqual List("Column: Country: Invalid ukDate(\"02/02/1966, 01/02/1966\"): at line: 3, column: 21") }
  }

  "succeed for xTime range with valid time" in {
    val schema =
      """version 1.0
           @totalColumns 1
           Country: xTime( 00:10:20 , 00:10:20 )"""

    parseAndValidate(new StringReader(schema)) must beLike {
      case SuccessZ(Schema(_, _))  => ok
    }
  }

  "fail for xTime range with invalid time" in {
    val schema =
      """version 1.0
           @totalColumns 1
           Country: xTime( 90:10:20 , 10:12:22 )"""

    parseAndValidate(new StringReader(schema)) must beLike { case FailureZ(msgs) => msgs.list mustEqual List("Column: Country: Invalid xTime(\"90:10:20, 10:12:22\"): at line: 3, column: 21") }
  }

  "fail for xTime range with from > to" in {
    val schema =
      """version 1.0
           @totalColumns 1
           Country: xTime( 00:10:21 , 00:10:20 )"""

    parseAndValidate(new StringReader(schema)) must beLike { case FailureZ(msgs) => msgs.list mustEqual List("Column: Country: Invalid xTime(\"00:10:21, 00:10:20\"): at line: 3, column: 21") }
  }

  "succeed for multiple date ranges with valid dates" in {
    val schema =
      """version 1.0
           @totalColumns 1
           Country: xTime( 00:10:20 , 00:10:20 ) ukDate( 01/02/1966 , 01/02/1966 ) xDateTime( 2012-12-01T01:01:01 , 2012-12-01T01:01:01 ) xDate( 2012-12-01 , 2012-12-01 )"""

    parseAndValidate(new StringReader(schema)) must beLike {
      case SuccessZ(Schema(_, _))  => ok
    }
  }

  "fail for multiple date ranges with invalid dates" in {
    val schema =
      """version 1.0
           @totalColumns 1
           Country: xTime( 99:10:20 , 00:10:20 ) ukDate( 01/02/1966 , 31/02/1966 ) xDateTime( 2012-13-01T01:01:01 , 2012-12-01T01:01:01 ) xDate( 2012-12-40 , 2012-12-01 )"""

    parseAndValidate(new StringReader(schema)) must beLike { case FailureZ(msgs) => msgs.list mustEqual List(
      """Column: Country: Invalid xTime("99:10:20, 00:10:20"): at line: 3, column: 21
        |Column: Country: Invalid ukDate("01/02/1966, 31/02/1966"): at line: 3, column: 50
        |Column: Country: Invalid xDateTime("2012-13-01T01:01:01, 2012-12-01T01:01:01"): at line: 3, column: 84
        |Column: Country: Invalid xDate("2012-12-40, 2012-12-01"): at line: 3, column: 139""".stripMargin) }
  }

  "succeed for cross reference in rule" in {
    val schema = """version 1.0
                      @totalColumns 2
                      Name: in($FullName)
                      FullName:"""

    parseAndValidate(new StringReader(schema)) must beLike {
      case SuccessZ(Schema(_, List(ColumnDefinition("Name", List(InRule(ColumnReference("FullName"))), _), _)))  => ok
    }
  }

  "succeed for regex and inRule rules on a single column" in {
    val schema = """version 1.0
                      @totalColumns 1
                      Name: regex ("[1-9][a-z]*") in("dog")"""

    parse(new StringReader(schema)) must beLike {
      case Success(Schema(_, List(ColumnDefinition("Name", List(RegexRule(r), InRule(Literal(Some(ir)))), _))), _) => {
        r mustEqual "[1-9][a-z]*"
        ir mustEqual "dog"
      }
    }
  }

  "succeed for inRule regex rules on a single and inRule has column reference and rules have had their order changed" in {
    val schema = """version 1.0
                      @totalColumns 1
                      Name: in($Name) regex("[1-9][a-z]*")"""

    parseAndValidate(new StringReader(schema)) must beLike {
      case SuccessZ(Schema(_, List(ColumnDefinition("Name", List(InRule(ColumnReference(ir)), RegexRule(r)), _)))) => {
        r mustEqual "[1-9][a-z]*"
        ir mustEqual "Name"
      }
    }
  }

  "succeed for 'is' text rule" in {
    val schema = """version 1.0
                      @totalColumns 1
                      Country: is("UK")"""

    parse(new StringReader(schema)) must beLike {
      case Success(Schema(_, List(ColumnDefinition("Country", List(IsRule(Literal(Some("UK")))), _))), _) => ok
    }
  }

  "succeed for 'is' cross reference rule" in {
    val schema = """version 1.0
                      @totalColumns 2
                      Country: is($MyCountry)
                      MyCountry:"""

    parseAndValidate(new StringReader(schema)) must beLike {
      case SuccessZ(Schema(_, List(ColumnDefinition("Country", List(IsRule(ColumnReference("MyCountry"))), _), ColumnDefinition("MyCountry", _, _)))) => ok
    }
  }

  "fail for invalid 'is' cross reference rule" in {
    val schema = """version 1.0
                   |@totalColumns 2
                   |Country: is($MyMissingCountry)
                   |MyCountry:""".stripMargin

    parseAndValidate(new StringReader(schema)) must beLike { case FailureZ(msgs) => msgs.list mustEqual List("Column: Country has invalid cross reference is($MyMissingCountry) at line: 3, column: 10") }
  }

  "fail for invalid regex rule and'is' cross reference rule" in {
    val schema = """version 1.0
                   |@totalColumns 2
                   |MyCountry: regex("[a-z*")
                   |Country: is($MyMissingCountry)""".stripMargin

    parseAndValidate(new StringReader(schema)) must beLike { case FailureZ(msgs) => msgs.list mustEqual List(
      """Column: Country has invalid cross reference is($MyMissingCountry) at line: 4, column: 10
        |Column: MyCountry: Invalid regex("[a-z*"): at line: 3, column: 12""".stripMargin) }
  }

  "succeed for 'isNot' text rule" in {
    val schema = """version 1.0
                      @totalColumns 1
                      Country: isNot("USA")"""

    parse(new StringReader(schema)) must beLike {
      case Success(Schema(_, List(ColumnDefinition("Country", List(IsNotRule(Literal(Some("USA")))), _))), _) => ok
    }
  }

  "succeed for 'isNot' cross reference rule" in {
    val schema = """version 1.0
                      @totalColumns 2
                      Country: isNot($MyCountry)
                      MyCountry:"""

    parseAndValidate(new StringReader(schema)) must beLike {
      case SuccessZ(Schema(_, List(ColumnDefinition("Country", List(IsNotRule(ColumnReference("MyCountry"))), _), ColumnDefinition("MyCountry", _, _)))) => ok
    }
  }

  "fail for invalid 'isNot' cross reference rule" in {
    val schema = """version 1.0
                   |@totalColumns 2
                   |Country: isNot($MyMissingCountry)
                   |MyCountry:""".stripMargin

    parseAndValidate(new StringReader(schema)) must beLike { case FailureZ(msgs) => msgs.list mustEqual List("Column: Country has invalid cross reference isNot($MyMissingCountry) at line: 3, column: 10") }
  }

  "succeed for 'starts' text rule" in {
    val schema = """version 1.0
                      @totalColumns 1
                      Country: starts("United")"""

    parse(new StringReader(schema)) must beLike {
      case Success(Schema(_, List(ColumnDefinition("Country", List(StartsRule(Literal(Some("United")))), _))), _) => ok
    }
  }

  "succeed for 'starts' cross reference rule" in {
    val schema = """version 1.0
                      @totalColumns 2
                      Country: starts($MyCountry)
                      MyCountry:"""

    parseAndValidate(new StringReader(schema)) must beLike {
      case SuccessZ(Schema(_,
      List(ColumnDefinition("Country", List(StartsRule(ColumnReference("MyCountry"))), _),
      ColumnDefinition("MyCountry", _, _)))) => ok
    }
  }

  "fail for invalid 'starts' cross reference rule" in {
    val schema = """version 1.0
                   |@totalColumns 2
                   |Country: starts($MyMissingCountry)
                   |MyCountry:""".stripMargin

    parseAndValidate(new StringReader(schema)) must beLike { case FailureZ(msgs) => msgs.list mustEqual List("Column: Country has invalid cross reference starts($MyMissingCountry) at line: 3, column: 10") }
  }

  "succeed for 'ends' text rule" in {
    val schema = """version 1.0
                      @totalColumns 1
                      Country: ends("Kingdom")"""

    parse(new StringReader(schema)) must beLike {
      case Success(Schema(_, List(ColumnDefinition("Country", List(EndsRule(Literal(Some("Kingdom")))), _))), _) => ok
    }
  }

  "succeed for 'ends' cross reference rule" in {
    val schema = """version 1.0
                      @totalColumns 2
                      Country: ends($MyCountry)
                      MyCountry:"""

    parseAndValidate(new StringReader(schema)) must beLike {
      case SuccessZ(Schema(_,  List(ColumnDefinition("Country", List(EndsRule(ColumnReference("MyCountry"))), _), ColumnDefinition("MyCountry", _, _)))) => ok
    }
  }

  "fail for invalid 'ends' cross reference rule" in {
    val schema = """version 1.0
                   |@totalColumns 2
                   |Country: ends($MyMissingCountry)
                   |MyCountry:""".stripMargin

    parseAndValidate(new StringReader(schema)) must beLike { case FailureZ(msgs) =>
      msgs.list mustEqual List("Column: Country has invalid cross reference ends($MyMissingCountry) at line: 3, column: 10") }
  }

  "succeed for multiple nested parens" in {
    val schema = """version 1.0
                      @totalColumns 2
                      Country: ends($MyCountry)  in("g") starts("st") or ends("en")
                      MyCountry:"""
    parseAndValidate(new StringReader(schema)) must beLike {
      case SuccessZ(_) => ok
    }
  }

  "fail for unmatched parens" in {
    val schema = """version 1.0
                      @totalColumns 2
                      Country: ends($MyCountry)  ((in("g") starts("st") or ends("en"))
                      MyCountry:"""
    parseAndValidate(new StringReader(schema)) must beLike {
      case FailureZ(msgs) => ok
    }
  }


  "Checksum crossreferences " should {
    "fail if column for filename is missing" in {
      val schema =
        """version 1.0
           @totalColumns 3 @noHeader
           Root:
           File:
           MD5: checksum(file($Root, $WRONG), "MD5")
        """

      parseAndValidate(new StringReader(schema)) must beLike { case FailureZ(msgs) => msgs.list mustEqual List("""Column: MD5 has invalid cross reference checksum(file($Root, $WRONG), "MD5") at line: 5, column: 17""") }
    }

    "fail if column for basePath is missing" in {
      val schema =
        """version 1.0
           @totalColumns 3 @noHeader
           Root:
           File:
           MD5: checksum(file($Hello, $File), "MD5")
        """

      parseAndValidate(new StringReader(schema)) must beLike { case FailureZ(msgs) => msgs.list mustEqual List("""Column: MD5 has invalid cross reference checksum(file($Hello, $File), "MD5") at line: 5, column: 17""") }
    }
  }


  "Range rule" should {
    "succeed with two integers" in {
      val schema = """version 1.0
                      @totalColumns 2
                      Age: range(0,100)
                      MyCountry:"""
      parseAndValidate(new StringReader(schema)) must beLike {
        case SuccessZ(_) => ok
      }
    }

    "succeed with two floats" in {
      val schema = """version 1.0
                      @totalColumns 2
                      Age: range(0.1,1.00)
                      MyCountry:"""
      parseAndValidate(new StringReader(schema)) must beLike {
        case SuccessZ(_) => ok
      }
    }

    "fail if min is greater than max" in {
      val schema = """version 1.0
                      @totalColumns 2
                      Age: range(100,1)
                      MyCountry:"""

      parseAndValidate(new StringReader(schema)) must beLike {
        case FailureZ(msgs) => msgs.list mustEqual List("""Column: Age: Invalid range, minimum greater than maximum in: 'range(100,1)' at line: 3, column: 28""")
      }
    }
  }

  "Length rule" should {

    "pass for single wildcard" in {
      val schema = """version 1.0
                      @totalColumns 1
                      Age: length(*)"""
      parseAndValidate(new StringReader(schema)) must beLike {
        case SuccessZ(_) => ok
      }
    }

    "pass for two wildcards" in {
      val schema = """version 1.0
                      @totalColumns 1
                      Age: length(*,*)"""
      parseAndValidate(new StringReader(schema)) must beLike {
        case SuccessZ(_) => ok
      }
    }

    "pass for single int" in {
      val schema = """version 1.0
                      @totalColumns 1
                      Age: length(12)"""
      parseAndValidate(new StringReader(schema)) must beLike {
        case SuccessZ(_) => ok
      }
    }

    "pass for ints" in {
      val schema = """version 1.0
                      @totalColumns 1
                      Age: length(1,33)"""
      parseAndValidate(new StringReader(schema)) must beLike {
        case SuccessZ(_) => ok
      }
    }

    "pass for wildcard and int" in {
      val schema = """version 1.0
                      @totalColumns 1
                      Age: length(*,55)"""
      parseAndValidate(new StringReader(schema)) must beLike {
        case SuccessZ(_) => ok
      }
    }

    "pass for int and wildcard" in {
      val schema = """version 1.0
                      @totalColumns 1
                      Age: length(4,*)"""
      parseAndValidate(new StringReader(schema)) must beLike {
        case SuccessZ(_) => ok
      }
    }

    "fail for single value that is not a wild card or int" in {
      val schema = """version 1.0
                      @totalColumns 1
                      Age: length(1.1)"""
      parseAndValidate(new StringReader(schema)) must beLike {
        case FailureZ(_) => ok
      }
    }

    "fail for invalid first value" in {
      val schema = """version 1.0
                      @totalColumns 1
                      Age: length(a,*)"""
      parseAndValidate(new StringReader(schema)) must beLike {
        case FailureZ(_) => ok
      }
    }

    "fail for invalid second value" in {
      val schema = """version 1.0
                      @totalColumns 1
                      Age: length(*,a)"""
      parseAndValidate(new StringReader(schema))must beLike {
        case FailureZ(_) => ok
      }
    }

    "fail for invalid second value" in {
      val schema = """version 1.0
                      @totalColumns 1
                      Age: length(100,1)"""

      parseAndValidate(new StringReader(schema)) must beLike {
        case FailureZ(messages) => messages.list mustEqual List("""Column: Age: Invalid length, minimum greater than maximum in: 'length(100,1)' at line: 3, column: 28""")
      }

    }

    "fail for invalid value in rule" in {
      val schema = """version 1.0
                      @totalColumns 1
                      Age: length(hello,1)"""

      parseAndValidate(new StringReader(schema)) must beLike {
        case FailureZ(_) => ok
      }

    }

    "succeed for for simplest if expr:" in {
      val schema = """version 1.0
                      @totalColumns 1 @noHeader
                      col1: if( length(1, 100) , starts("start") ) """
      parseAndValidate(new StringReader(schema)) must beLike {
        case SuccessZ(_) => ok
      }
    }

    "succeed for if with optional else :" in {
      val schema = """version 1.0
                      @totalColumns 1 @noHeader
                      col1: if( length(1, 100) , starts("start"), ends("ends") ) """
      parseAndValidate(new StringReader(schema)) must beLike {
        case SuccessZ(_) => ok
      }
    }

    "succeed for if with nested if :" in {
      val schema = """version 1.0
                      @totalColumns 1 @noHeader
                      col1: if( length(1, 100) , if( starts("start"), ends("ends"), ends("start")) ) """
      parseAndValidate(new StringReader(schema)) must beLike {
        case SuccessZ(_) => ok
      }
    }

    "succeed for if with nested if and else nested :" in {
      val schema = """version 1.0
                      @totalColumns 1 @noHeader
                      col1: if( length(1, 100) , if( starts("start"), ends("ends"), ends("start")) ,  if( starts("start"), ends("ends"), ends("start")) ) """
      parseAndValidate(new StringReader(schema)) must beLike {
        case SuccessZ(_) => ok
      }
    }

    "fail for if when condition is an if expr :" in {
      val schema = """version 1.0
                      @totalColumns 1 @noHeader
                      col1: if( if( starts("start"), ends("ends")) ,  ends("start") )"""
      parseAndValidate(new StringReader(schema)) must beLike {
        case FailureZ(_) => ok
      }
    }

    "fail for if when only condition supplied :" in {
      val schema = """version 1.0
                      @totalColumns 1 @noHeader
                      col1: if( starts("start") )"""
      parseAndValidate(new StringReader(schema)) must beLike {
        case FailureZ(_) => ok
      }
    }

    "fail for if when more than two expressions supplied :" in {
      val schema = """version 1.0
                      @totalColumns 1 @noHeader
                      col1: if( starts("start"), ends("end") , ends("end"), ends("end"))"""
      parseAndValidate(new StringReader(schema)) must beLike {
        case FailureZ(_) => ok
      }
    }


  }

  "unique multi-column should" should {
    "succeed with two integers" in {
      val schema = """version 1.0
                      @totalColumns 3
                      Name: unique( $Age, $PostCode )
                      Age:
                      PostCode:
                   """
      parseAndValidate(new StringReader(schema)) must beLike {
        case SuccessZ(_) => ok
      }
    }

    "fail if the cross reference column is invalid" in {
      val schema = """version 1.0
                      @totalColumns 3
                      Name: unique( $MADEUP, $PostCode )
                      Age:
                      PostCode:
                   """
      parseAndValidate(new StringReader(schema)) must beLike {
        case FailureZ(messages) => messages.list mustEqual List("""Column: Name: Invalid cross reference $MADEUP: at line: 3, column: 29""")
      }
    }



  }
}