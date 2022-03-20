/*
 * Copyright (c) 2013, The National Archives <digitalpreservation@nationalarchives.gov.uk>
 * https://www.nationalarchives.gov.uk
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package uk.gov.nationalarchives.csv.validator.schema.v1_1

import java.io.FileNotFoundException
import uk.gov.nationalarchives.csv.validator.Util.FileSystem
import uk.gov.nationalarchives.csv.validator.metadata.Row
import uk.gov.nationalarchives.csv.validator.schema._
import uk.gov.nationalarchives.csv.validator.schema.v1_0.{DateRangeRule, IsoDateTimeParser}

import scala.util.Try
import scalaz.Scalaz._
import scalaz.{Failure => FailureZ, Success => SuccessZ}

import java.nio.file.Path





case class AnyRule(anyValues: List[ArgProvider]) extends Rule("any", anyValues:_*) {
  override def valid(cellValue: String, columnDefinition: ColumnDefinition, columnIndex: Int, row: Row, schema: Schema, mayBeLast: Option[Boolean]  = None): Boolean = {

    val ruleValues = (for (rule <- anyValues) yield rule.referenceValue(columnIndex, row, schema)).toList.flatten

    val (rv, cv) = if (columnDefinition.directives.contains(IgnoreCase())) (ruleValues.map(_.toLowerCase), cellValue.toLowerCase) else (ruleValues, cellValue)
    rv.contains(cv)
  }
}


case class SwitchRule(elseRules: Option[List[Rule]], cases:(Rule, List[Rule])*) extends Rule("switch") {


  override def evaluate(columnIndex: Int, row: Row, schema: Schema, mayBeLast: Option[Boolean] = None): RuleValidation[Any] = {
    def conditionValid(condition: Rule): Boolean = {
      val (cellValue,idx) = findColumnRefence(condition) match {
        case Some(columnRef) =>
          (columnRef.referenceValueEx(columnIndex, row, schema), columnIdentifierToIndex(schema, columnRef.ref))
        case None =>
          (row.cells(columnIndex).value, columnIndex)
      }
      condition.valid(cellValue, schema.columnDefinitions(columnIndex), idx, row, schema)
    }

    cases.collectFirst { case (condition, rules) if (conditionValid(condition)) =>
      for (rule <- rules) yield {
        rule.evaluate(columnIndex, row, schema)
      }
    }.getOrElse{
      elseRules.map{ er =>
        for (rule <- er) yield {
          rule.evaluate(columnIndex, row, schema)
        }
      }.getOrElse(Nil)
    }.sequence[RuleValidation, Any]
  }


  override def toError = {
    val paramErrs = cases.flatMap{ case (_,rules) => rules.map( _.toError).mkString(" ")}
    s"""($paramErrs)""" + (if (argProviders.isEmpty) "" else "(" + argProviders.foldLeft("")((a, b) => (if (a.isEmpty) "" else a + ", ") + b.toError) + ")")
  }
}




case class IntegrityCheckRule(pathSubstitutions: List[(String,String)], enforceCaseSensitivePathChecks: Boolean, rootPath: ArgProvider = Literal(None), topLevelFolder: String = "content", includeFolder: Boolean = false) extends Rule("integrityCheck", Seq(rootPath): _*) {

  //TODO introduce state, not very functional
  var filesMap = Map[String, Set[Path]]()

  override def evaluate(columnIndex: Int, row: Row,  schema: Schema, mayBeLast: Option[Boolean] = None): RuleValidation[Any] = {
    try{
      if (valid(cellValue(columnIndex, row, schema), schema.columnDefinitions(columnIndex), columnIndex, row, schema, mayBeLast)) true.successNel[String] else fail(columnIndex, row, schema)
    }
    catch {
      case ex: FileNotFoundException =>
        val columnDefinition = schema.columnDefinitions(columnIndex)
        s"$toError fails for line: ${row.lineNumber}, column: ${columnDefinition.id}, ${ex.getMessage} with substitution paths ${pathSubstitutions.mkString(", ")}".failureNel[Any]
    }
  }

  override def valid(filePath: String, columnDefinition: ColumnDefinition, columnIndex: Int, row: Row, schema: Schema, mayBeLast: Option[Boolean]): Boolean = {

    if (!filePath.isEmpty){

        val ruleValue = rootPath.referenceValue(columnIndex, row, schema)

        filesMap = new FileSystem(ruleValue, filePath, pathSubstitutions).integrityCheck(filesMap, enforceCaseSensitivePathChecks, topLevelFolder, includeFolder)
        val isLastLine = mayBeLast.map(!_).getOrElse(false)

        if (isLastLine)
        { filesMap.forall{case (folder,files) => files.isEmpty} }
        else
          true
    }
    else
      false
  }

  override def toError = {  s"""$ruleName""" + (if (rootPath == Literal(None)) "" else s"""(${rootPath.toError})""") }

  override def toValueError(row: Row, columnIndex:Int ) =  {

    val extraFiles = filesMap.collect{ case (folder,files) if files.nonEmpty =>
      files.mkString(", ")
    }.mkString(", ")

    s"""files: ${'"'}$extraFiles${'"'} are not listed in the metadata"""
  }
}


case class XsdDateTimeWithTimeZoneRule() extends DateRule("xDateTimeWithTimeZone", XsdDateTimeWithTimeZoneRegex, IsoDateTimeParser)

case class XsdDateTimeWithTimeZoneRangeRule(from: String, to: String) extends DateRangeRule("xDateTimeWithTimeZone", XsdDateTimeWithTimeZoneRegex, IsoDateTimeParser)


case class UpperCaseRule() extends PatternRule("upperCase", UpperCaseRegex)

case class LowerCaseRule() extends PatternRule("lowerCase", LowerCaseRegex)

case class IdenticalRule() extends Rule("identical") {

  var lastValue: Option[String] = None

  override def valid(cellValue: String, columnDefinition: ColumnDefinition, columnIndex: Int, row: Row, schema: Schema,  mayBeLast: Option[Boolean] = None): Boolean = {
    if (cellValue.isEmpty) false
    else if (lastValue.isEmpty){
      lastValue = Some(cellValue)
      true
    }
    else{
      val res = cellValue.equals(lastValue.getOrElse(""))
      lastValue = Some(cellValue)
      res
    }
  }
}



case class RangeRule(min: Option[BigDecimal], max: Option[BigDecimal]) extends Rule("range") {

  override def valid(cellValue: String, columnDefinition: ColumnDefinition, columnIndex: Int, row: Row, schema: Schema,  mayBeLast: Option[Boolean] = None): Boolean = {

    Try[BigDecimal]( BigDecimal(cellValue)) match {

      case scala.util.Success(callDecimal) =>
        min.map( callDecimal >= _).getOrElse(true) &&  max.map( callDecimal <= _).getOrElse(true)
      case _ => false
    }
  }

  override def toError = s"""$ruleName(${min.getOrElse("*")},${max.getOrElse("*")})"""
}




