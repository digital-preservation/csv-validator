/**
 * Copyright (c) 2013, The National Archives <digitalpreservation@nationalarchives.gov.uk>
 * http://www.nationalarchives.gov.uk
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package uk.gov.nationalarchives.csv.validator.schema

import uk.gov.nationalarchives.csv.validator.schema.v1_0.{SchemaParser => SchemaParser1_0}
import uk.gov.nationalarchives.csv.validator.schema.v1_1.{SchemaParser => SchemaParser1_1, _}
import uk.gov.nationalarchives.csv.validator.schema.v1_2.{SchemaParser => SchemaParser1_2, _}

import scala.util.parsing.combinator._
import scala.language.reflectiveCalls
import java.io.Reader

import scalaz._
import Scalaz._

import uk.gov.nationalarchives.csv.validator.{SchemaDefinitionError, FailMessage}

/**
  * CSV Schema Parser
  *
  * Uses Scala Parser Combinators to parse the CSV Schema language defined in
  * the specification document
  *
  * @see http://digital-preservation.github.io/csv-validator/csv-schema-1.0.html
  */
trait SchemaParser extends RegexParsers
with PackratParsers
with TraceableParsers {

  /**
    * Any path substitutions needed when
    * resolving file paths
    */
  val pathSubstitutions: List[(String, String)]

  /**
    * Whether to enforce case sensitivity
    * in file path checks. Useful
    * when working on operating systems /
    * filesystems that ignore file path
    * case sensitivity, e.g. Windows and NTFS
    */
  val enforceCaseSensitivePathChecks: Boolean



  lazy val versionHeader: PackratParser[String] = "VersionDecl" ::= ("version" ~> versionLiteral )


  lazy val versionLiteral: Parser[String] = "VersionLiteral" ::= """[0-9\.]*""".r



  /**
    * Given 1 or more Parsers
    * this function produces
    * all permutations of
    * all combinations.
    *
    * Put more simply if you have a List
    * of Parsers, we create a Parser
    * that matches n of those parsers
    * in any order
    *
    * @param parsers A list of parsers to mingle
    * @return A parser that represents all permutations of
    *         all combinations of the parsers
    */
  protected def mingle[T, U](parsers : List[Parser[T]]): Parser[List[T]] = {

    /**
      * All permutations of all combinations
      * of a List
      */
    def mingle[T](data: List[T]): List[List[T]] = {
      (for(i <- 1 to data.length) yield
        data.combinations(i).flatMap(_.permutations)
        ).toList.flatten
    }

    /**
      * Combines n parsers together
      * in the same manner as p1 ~ p2 ~ ... pN
      */
    def combine[T](parsers: List[Parser[T]]): Parser[List[T]] = {
      parsers.foldRight(success(List.empty[T])) {
        case (p, acc) => for {
          pRes <- p
          accRes <- acc
        } yield pRes :: accRes
      }
    }

    def longestFirst(l1: List[_], l2: List[_]) = l1.length > l2.length

    val mingled = mingle[Parser[T]](parsers)
      .sortWith(longestFirst)
    //we sort longest first here,
    //to make sure the parser that matches
    //the most input will always be put first

    val alternates = mingled.map(combine(_))
    alternates.reduceLeft(_ | _)
  }




  def parseAndValidate(reader: Reader): ValidationNel[FailMessage, Schema] = {
    //TODO following function works around a deficiency in scala.util.parsing.combinator.Parsers{$Error, $Failure} that use hard-coded Unix EOL in Scala 2.10.0
    def formatNoSuccessMessageForPlatform(s: String) = {
      if(sys.props("os.name").toLowerCase.startsWith("win"))
        s.replaceAll("([^\\r]?)\\n", "$1\r\n")
      else
        s
    }

    parse(reader) match {
      case s @ Success(schema: Schema, next) => {
        val errors = SchemaValidator.validate(schema)
        if (errors.isEmpty) schema.successNel[FailMessage] else FailMessage(SchemaDefinitionError, errors).failureNel[Schema]
      }
      case n: NoSuccess => FailMessage(SchemaDefinitionError, formatNoSuccessMessageForPlatform(n.toString)).failureNel[Schema]
    }
  }

  private def parseSchemaVersion(reader: Reader): (ParseResult[String], Reader) = {
    import java.io.BufferedReader

    val br = new BufferedReader(reader)
    br.mark(1024)
    val versionLine = br.readLine()
    br.reset()

    (parseAll(versionHeader,versionLine), br)
  }

  private def parse(reader: Reader, version: String, next: Input): ParseResult[Schema] = {
    val ecspc = enforceCaseSensitivePathChecks
    val ps =  pathSubstitutions
    val t = trace

    SchemaValidator.versionValid(version).map(Failure(_, next)).getOrElse {
      version match {
        case "1.2" =>
          val parser1_2 = new SchemaParser1_2 {override val enforceCaseSensitivePathChecks: Boolean = ecspc
            override val pathSubstitutions: List[(String, String)] = ps
            override val trace: Boolean = t
          }

          parser1_2.parseVersionAware(reader) match {
            case parser1_2.Success(s, n) => Success(s, n)
            case parser1_2.Failure(msg, n) => Failure(msg, n)
            case parser1_2.Error(msg, n) => Error(msg, n)
          }

        case "1.1" =>
          val parser1_1 = new SchemaParser1_1 {override val enforceCaseSensitivePathChecks: Boolean = ecspc
            override val pathSubstitutions: List[(String, String)] = ps
            override val trace: Boolean = t
          }

          parser1_1.parseVersionAware(reader) match {
            case parser1_1.Success(s, n) => Success(s, n)
            case parser1_1.Failure(msg, n) => Failure(msg, n)
            case parser1_1.Error(msg, n) => Error(msg, n)
          }

        case _ =>
          val parser1_0 = new SchemaParser1_0 {override val enforceCaseSensitivePathChecks: Boolean = ecspc
            override val pathSubstitutions: List[(String, String)] = ps
            override val trace: Boolean = t
          }

          parser1_0.parseVersionAware(reader) match {
            case parser1_0.Success(s, n) => Success(s, n)
            case parser1_0.Failure(msg, n) => Failure(msg, n)
            case parser1_0.Error(msg, n) => Error(msg, n)
          }
      }
    }
  }


  def parse(reader: Reader): ParseResult[Schema] = {
    val (parseResult, br)  = parseSchemaVersion(reader)
    parseResult match {
      case s@Success(version: String, next) =>
        parse(br, version, next)
      case n: NoSuccess => n
    }
  }


}





