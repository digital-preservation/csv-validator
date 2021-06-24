/**
 * Copyright (c) 2013, The National Archives <digitalpreservation@nationalarchives.gov.uk>
 * https://www.nationalarchives.gov.uk
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package uk.gov.nationalarchives.csv.validator.schema

import scala.util.parsing.combinator.Parsers
import scala.language.implicitConversions

/**
 * TraceableParsers provides the Traceable
 * Parser `::=` which allows you to name parsers
 * and also to trace their parse attempts.
 *
 * The `::=` symbol was chosen as it aligns with
 * the syntax of EBNF which also leads to the
 * nice conincidence that your parser definitions
 * will likely look closer to their EBNF counterpart
 * definitions.
 *
 * @author Adam Retter <adam.retter@googlemail.com>
 */
trait TraceableParsers extends Parsers {

  val trace: Boolean //whether tracing should be enabled or not

  class TraceableParser[+T](parser: Parser[T]) extends Parser[T] {
    def apply(in: Input): ParseResult[T] = {
      if(trace) {
        val first = in.first
        val pos = in.pos
        val offset = in.offset
        val parseResult = parser.apply(in)
        println(s"""${parser.toString()}.apply for token "$first" at position $pos offset $offset returns "$parseResult"""")
        parseResult
      } else {
        parser.apply(in)
      }
    }
  }

  /**
   * Implicit which converts a name and a parser into a TraceableParser
   *
   * e.g. Give the EBNF:
   *  <code><pre>MyParser ::= "SomeValue"?</pre></code>
   *
   * You would write the Scala code:
   *  <code><pre>val myParser : Option[Parser[String]] = "MyParser" ::= "SomeValue"?</pre></code>
   */
  implicit def toTraceableParser(name: String) = new {
    def ::=[T](p: Parser[T]) = new TraceableParser(p.named(name))
  }
}
