package uk.gov.tna.dri.csv

import util.parsing.combinator.JavaTokenParsers

/**
 * User: Jim Collins
 * Date: 1/15/13
 */
class CsvParser  extends JavaTokenParsers {

  def schema: Parser[Any] = (
    "{"
      ~opt(comment)
      ~opt(globalDirective)
      ~opt(comment)
      ~opt(definition)
      ~opt(comment)
      ~"}"
    )

  /*
  * Global Definition
   */
  def globalDirective: Parser[Any] = (
    "globalDirective"~"{"
      ~opt(globalDirectives)
      ~"}"
    )

  def globalDirectives: Parser[Any] = (
    opt(separatorDirective)
      ~opt(quotedDirective)
      ~opt(totalColumnsDirective)
      ~opt(noHeaderDirective | ignoreColumnNameCase)
    )



  def separatorDirective: Parser[Any] = "@Separator:"~charLiteral

  def quotedDirective: Parser[Any] = "@Quoted:"~(tabExpr | charLiteral)

  def tabExpr: Parser[Any] = "TAB("~wholeNumber~")"

  def totalColumnsDirective: Parser[Any] = "@TotalColumns:"~positiveNumber

  def noHeaderDirective: Parser[Any] = "@NoHeader"

  def ignoreColumnNameCase: Parser[Any] = "@IgnoreColumnNameCase"

  /*
  * Column Definition
   */

  def definition: Parser[Any] = "columnDefinitions"~"{"~opt(rep(columnDefinition))~"}"

  def columnDefinition: Parser[Any] = (
    opt(comment)~columnIdentifier~"{"~opt(columnValidationExpr)~opt(columnDirectives)~"}"
    )

  def columnIdentifier: Parser[Any] = ident | positiveNumber

  def columnValidationExpr: Parser[Any] = "@ColumnValidationExpr:"~stringLiteral

  def columnDirectives: Parser[Any] = (
    opt(ignoreCaseDirective)
      ~opt(matchIsFalseDirective)
      ~opt(notEmptyDirective)
      ~opt(uniqueDirective)
    )

  def ignoreCaseDirective: Parser[Any] = "@IgnoreCase"

  def matchIsFalseDirective: Parser[Any] = "@MatchIsFalse"

  def notEmptyDirective: Parser[Any] = "@NotEmpty"

  def uniqueDirective: Parser[Any] = "@Unique"

  /*
  * General definitions
   */

  def comment: Parser[Any] = singleLineComment | multiLineComment

  def singleLineComment: Parser[Any] = """//.*""".r

  def multiLineComment: Parser[Any] = """(?:/\*(?:[^*]|(?:\*+[^*/]))*\*+/)|(?://.*)""".r

  def charLiteral: Parser[String] = """[\w,;]""".r

  def positiveNumber: Parser[String] = """\d+""".r
}

object CsvParser  {
  def main(args: Array[String]) {
    val csvParser = new CsvParser
    val res = csvParser.parseAll(csvParser.schema,
      """
        {//a single line comment{

          globalDirective{
            @Separator: ;

            @TotalColumns: 100
            @NoHeader

          }
          /*
            *
            *a multi line comment
            *
          */
        columnDefinitions{
          column1{
            @ColumnValidationExpr: "jimbo was here"
            @MatchIsFalse
          }

          column2{ @NotEmpty @Unique }
        }
        }
      """.stripMargin)
    println(res)
  }
}
