/**
 * Copyright (c) 2013, The National Archives <digitalpreservation@nationalarchives.gov.uk>
 * https://www.nationalarchives.gov.uk
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package uk.gov.nationalarchives.csv.validator.schema.v1_1

import uk.gov.nationalarchives.csv.validator._
import uk.gov.nationalarchives.csv.validator.schema.v1_0.{SchemaValidator => SchemaValidator1_0}
import uk.gov.nationalarchives.csv.validator.schema.{ColumnDefinition, GlobalDirective, Rule}

/**
  * Set of rules to validate the schema (compatible with the CSV Schema 1.1)
  * Overrides 1.0 rules for backward compatibilities and update only the rules who needs to be
  *  @author Valy Dia
  */
class SchemaValidator extends SchemaValidator1_0 {

  override protected def rangeValid(columnDefinitions: List[ColumnDefinition]): Option[String] = {
    def rangeCheck(rule: Rule): Option[String] = rule match {
      case RangeRule(None,None) =>  Some(s"""Invalid range in 'range(*,*)' at least one value needs to be defined""")
      case RangeRule(Some(min),Some(max)) => if (min > max) Some(s"""Invalid range, minimum greater than maximum in: 'range($min,$max)' at line: ${rule.pos.line}, column: ${rule.pos.column}""") else None
      case _ => None
    }

    val v = for {
      cd <- columnDefinitions
      rule <- cd.rules
      message = rangeCheck(rule)
      if (message.isDefined)
    } yield {
      val errormessage = rule match {
        case range:RangeRule => message.getOrElse("")
        case _ =>  s"""Column: ${cd.id}: Invalid range, minimum greater than maximum: at line: ${rule.pos.line}, column: ${rule.pos.column}"""
      }
      s"""Column: ${cd.id}: """ + errormessage

    }

    if (v.isEmpty) None else Some(v.mkString(EOL))
  }

}

object SchemaValidator {
  def apply(g: List[GlobalDirective], c: List[ColumnDefinition]): String = {
    val parser = new SchemaValidator()
    parser.validate(g,c)
  }
}