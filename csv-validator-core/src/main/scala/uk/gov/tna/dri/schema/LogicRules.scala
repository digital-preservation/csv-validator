/*
 * Copyright (c) 2013, The National Archives digitalpreservation@nationalarchives.gov.uk
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package uk.gov.tna.dri.schema

import scala.Some
import scalaz.{Success => SuccessZ, Failure => FailureZ}

import scalaz.Scalaz._
import uk.gov.tna.dri.metadata.Row


case class OrRule(left: Rule, right: Rule) extends Rule("or") {
  override def evaluate(columnIndex: Int, row: Row, schema: Schema): RuleValidation[Any] = {
    left.evaluate(columnIndex, row, schema) match {
      case s @ SuccessZ(_) => s

      case FailureZ(_) => right.evaluate(columnIndex, row, schema) match {
        case s @ SuccessZ(_) => s
        case FailureZ(_) => fail(columnIndex, row, schema)
      }
    }
  }

  def valid(cellValue: String, columnDefinition: ColumnDefinition, columnIndex: Int, row: Row, schema: Schema): Boolean = {
    evaluate(columnIndex, row, schema) match {
      case FailureZ(_) => false
      case SuccessZ(_) => true
    }
  }

  override def toError = s"""${left.toError} $ruleName ${right.toError}"""
}

case class ParenthesesRule(rules: List[Rule]) extends Rule("parentheses") {

  override def evaluate(columnIndex: Int, row: Row, schema: Schema): RuleValidation[Any] = {
    val v = for (rule <- rules) yield {
      rule.evaluate(columnIndex, row, schema)
    }

    v.sequence[RuleValidation, Any]
  }

  def valid(cellValue: String, columnDefinition: ColumnDefinition, columnIndex: Int, row: Row, schema: Schema): Boolean = {
    evaluate(columnIndex, row, schema) match {
      case FailureZ(_) => false
      case SuccessZ(_) => true
    }
  }

  override def toError = {
    val paramErrs = rules.map(_.toError).mkString(" ")
    s"""($paramErrs)""" + (if (argProviders.isEmpty) "" else "(" + argProviders.foldLeft("")((a, b) => (if (a.isEmpty) "" else a + ", ") + b.toError) + ")")
  }
}

case class IfRule(condition: Rule, rules: List[Rule], elseRules: Option[List[Rule]]) extends Rule("if") {

  override def evaluate(columnIndex: Int, row: Row, schema: Schema): RuleValidation[Any] = {
    val (cellValue,idx) = condition.explicitColumn match {
      case Some(columnName) => (row.cells(columnNameToIndex(schema, columnName)).value, columnNameToIndex(schema, columnName) )
      case None => (row.cells(columnIndex).value, columnIndex)
    }

    val v = if (condition.valid(cellValue, schema.columnDefinitions(columnIndex), idx, row, schema)) {
     for (rule <- rules) yield {
        rule.evaluate(columnIndex, row, schema)
      }
    } else {
      if (elseRules.isDefined) {
        for (rule <- elseRules.get) yield {
          rule.evaluate(columnIndex, row, schema)
        }
      } else {
        Nil
      }
    }

    v.sequence[RuleValidation, Any]
  }

  def valid(cellValue: String, columnDefinition: ColumnDefinition, columnIndex: Int, row: Row, schema: Schema): Boolean = {
    evaluate(columnIndex, row, schema) match {
      case FailureZ(_) => false
      case SuccessZ(_) => true
    }
  }

  override def toError = {
    val paramErrs = rules.map( _.toError).mkString(" ")
    s"""($paramErrs)""" + (if (argProviders.isEmpty) "" else "(" + argProviders.foldLeft("")((a, b) => (if (a.isEmpty) "" else a + ", ") + b.toError) + ")")
  }
}

