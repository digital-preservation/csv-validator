/**
 * Copyright (c) 2013, The National Archives <digitalpreservation@nationalarchives.gov.uk>
 * http://www.nationalarchives.gov.uk
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package uk.gov.nationalarchives.csv.validator.schema.v1_0

import java.security.MessageDigest

import uk.gov.nationalarchives.csv.validator._
import uk.gov.nationalarchives.csv.validator.schema._

import scala.annotation.tailrec
import scala.collection.immutable.TreeMap
import scala.util.Try

/**
 * Set of rules to validate the schema (compatible with the CSV Schema 1.0)
 *  @author Valy Dia
 */
class SchemaValidator {
 def validate(g: List[GlobalDirective], c: List[ColumnDefinition]): String = {
   globDirectivesValid(g) ::totalColumnsValid(g, c) :: columnDirectivesValid(c) :: duplicateColumnsValid(c) :: crossColumnsValid(c) :: checksumAlgorithmValid(c) ::
     rangeValid(c) :: lengthValid(c) :: regexValid(c) :: dateRangeValid(c) :: uniqueMultiValid(c) :: explicitColumnValid(c) :: Nil collect { case Some(s: String) => s } mkString(EOL)
 }

 protected def totalColumnsValid(g: List[GlobalDirective], c: List[ColumnDefinition]): Option[String] = {
   val tc: Option[TotalColumns] = g.collectFirst { case t @ TotalColumns(_) => t }

   if (!tc.isEmpty && tc.get.numberOfColumns != c.length)
     Some(s"@totalColumns = ${tc.get.numberOfColumns} but number of columns defined = ${c.length} at line: ${tc.get.pos.line}, column: ${tc.get.pos.column}" )
   else
     None
 }


 protected def duplicateColumnsValid(columnDefinitions: List[ColumnDefinition]): Option[String] = {
   val duplicates = TreeMap(columnDefinitions.groupBy(_.id).toSeq:_*)(scala.math.Ordering.by[ColumnIdentifier, String](_.toString)).filter(_._2.length > 1)

   if (duplicates.isEmpty) None
   else Some(duplicates.map { case (id, cds) => s"""Column: $id has duplicates on lines """ + cds.map(cd => cd.pos.line).mkString(", ") }.mkString(EOL))
 }

 protected def globDirectivesValid(directives: List[GlobalDirective]): Option[String] = {
   val duplicates = for ((name, lst) <- directives.groupBy(_.name) if (lst.length > 1)) yield {
     s"Global directive @$name is duplicated"
   }

   if (duplicates.isEmpty) None else Some(duplicates.mkString(EOL))
 }

 protected def columnDirectivesValid(columnDefinitions: List[ColumnDefinition]): Option[String] = {
   val v = for {
     cd <- columnDefinitions
     if (cd.directives.distinct.length != cd.directives.length)
   } yield {
     s"${cd.id}: Duplicated column directives: " +
       cd.directives.groupBy(identity).filter { case (_, cds) => cds.size > 1}.map { case (cdId, _) => "@" + cdId + s" at line: ${cdId.pos.line}, column: ${cdId.pos.column}"}.mkString(", ")
   }

   if (v.isEmpty) None else Some(v.mkString(EOL))
 }

 protected def checksumAlgorithmValid(columnDefinitions: List[ColumnDefinition]): Option[String] = {

   def algorithmCheck(rule: Rule): Boolean = rule match {
     case checksum:ChecksumRule => Try(
       // 3rd party checksum algorithm may need to be checked here
       // if not added a part of the Service Provider Interface (SPI)
       MessageDigest.getInstance(checksum.algorithm)
     ).isFailure

     case _ => false
   }

   val v = for {
     cd <- columnDefinitions
     rule <- cd.rules
     if (algorithmCheck(rule))
   } yield {
     rule match {
       case checksum:ChecksumRule => s"""Column: ${cd.id}: Invalid Algorithm: '${checksum.algorithm}' at line: ${rule.pos.line}, column: ${rule.pos.column}"""
       case _ =>  s"""Column: ${cd.id}: Invalid Algorithm: at line: ${rule.pos.line}, column: ${rule.pos.column}"""
     }
   }

   if (v.isEmpty) None else Some(v.mkString(EOL))
 }

 protected def rangeValid(columnDefinitions: List[ColumnDefinition]): Option[String] = {
   def rangeCheck(rule: Rule): Boolean = rule match {
     case RangeRule(min,max) => min > max
     case _ => false
   }

   val v = for {
     cd <- columnDefinitions
     rule <- cd.rules
     if (rangeCheck(rule))
   } yield {
     rule match {
       case range:RangeRule => s"""Column: ${cd.id}: Invalid range, minimum greater than maximum in: 'range(${range.min},${range.max})' at line: ${rule.pos.line}, column: ${rule.pos.column}"""
       case _ =>  s"""Column: ${cd.id}: Invalid range, minimum greater than maximum: at line: ${rule.pos.line}, column: ${rule.pos.column}"""
     }
   }

   if (v.isEmpty) None else Some(v.mkString(EOL))
 }

 protected def lengthValid(columnDefinitions: List[ColumnDefinition]): Option[String] = {
   def lengthCheck(rule: Rule): Boolean = rule match {
     case LengthRule(Some(from),to) => if (from == "*" || to == "*") true else from.toInt <= to.toInt
     case _ => true
   }

   val v = for {
     cd <- columnDefinitions
     rule <- cd.rules
     if (!lengthCheck(rule))
   } yield {
     rule match {
       case len:LengthRule => s"""Column: ${cd.id}: Invalid length, minimum greater than maximum in: 'length(${len.from.getOrElse("")},${len.to})' at line: ${rule.pos.line}, column: ${rule.pos.column}"""
       case _ =>  s"""Column: ${cd.id}: Invalid length, minimum greater than maximum: at line: ${rule.pos.line}, column: ${rule.pos.column}"""
     }
   }

   if (v.isEmpty) None else Some(v.mkString(EOL))
 }

 protected def crossColumnsValid(columnDefinitions: List[ColumnDefinition]): Option[String] = {

   def getAllRules(cds:ColumnDefinition): List[Rule] = {

     @tailrec
     def getAllRules(rules: List[Rule], acc: List[Rule]): List[Rule] = rules match {
       case Nil => acc
       case rule :: tail => rule match {
           case AndRule(left, right) =>
             getAllRules(tail :+ left :+ right, acc)
           case OrRule(left, right) =>
             getAllRules(tail :+ left :+ right, acc)
           case _ => getAllRules(tail, acc :+ rule)
         }
     }
     getAllRules(cds.rules, List())
   }

   def checkRuleIsReferenced(rule: Rule): Boolean = {
     def undefinedCross(a: ArgProvider) = a match {
         case ColumnReference(name) => !columnDefinitions.exists(col => col.id == name)
         case _ => false
     }
     rule.argProviders.foldLeft(false)((acc, arg) =>  acc || undefinedCross(arg))
   }


   def filterRules(cds: ColumnDefinition ): List[Rule] = { // List of failing rules
     getAllRules(cds).filter(rule => {
       def undefinedCross(a: ArgProvider) = a match {
         case ColumnReference(name) => !columnDefinitions.exists(col => col.id == name)
         case _ => false
       }

       rule.argProviders.foldLeft(false)((acc, arg) => acc || undefinedCross(arg))
     })
   }

   def crossReferenceErrors(rules: List[Rule]): String = {
     val errors = rules collect { case rule: Rule => s"""${rule.toError} at line: ${rule.pos.line}, column: ${rule.pos.column}""" }

     (if (errors.length == 1) "cross reference " else "cross references ") + errors.mkString(", ")
   }

   val errors = columnDefinitions.map(cd => (cd, filterRules(cd))).filter(_._2.length > 0)

   if (errors.isEmpty) None
   else Some(errors.map { case (cd, rules) => s"Column: ${cd.id} has invalid ${crossReferenceErrors(rules)}" }.mkString(EOL))
 }

 protected def regexValid(columnDefinitions: List[ColumnDefinition]): Option[String] = {
   def regexCheck(rule: Rule): Boolean = rule match {
     case RegExpRule(s) =>  Try(s.r).isFailure
     case _ => false
   }

   val result = for {
     cd <- columnDefinitions
     rule <- cd.rules
     if (regexCheck(rule))
   } yield s"""Column: ${cd.id}: Invalid ${rule.toError}: at line: ${rule.pos.line}, column: ${rule.pos.column}"""

   if (result.isEmpty) None else Some(result.mkString(EOL))
 }

 protected def dateRangeValid(columnDefinitions: List[ColumnDefinition]): Option[String] = {

   def dateCheck(rule: Rule): Boolean = rule match {
     case dateRule: DateRangeRule =>  {
       val diff = for (frmDt <- dateRule.fromDate; toDt <- dateRule.toDate) yield frmDt.isBefore(toDt) || frmDt.isEqual(toDt)

       diff match {
         case scala.util.Success(false) => false
         case scala.util.Success(true) => true
         case scala.util.Failure(_) => false
       }
     }
     case _ => true
   }

   val result = for {
     cd <- columnDefinitions
     rule <- cd.rules
     if (!dateCheck(rule))
   } yield s"""Column: ${cd.id}: Invalid ${rule.toError}: at line: ${rule.pos.line}, column: ${rule.pos.column}"""

   if (result.isEmpty) None else Some(result.mkString(EOL))
 }

 protected def uniqueMultiValid(columnDefinitions: List[ColumnDefinition]): Option[String] = {
   def uniqueMultiCheck(rule: Rule): Option[List[ColumnIdentifier]] = rule match {
     case UniqueMultiRule(columns) =>
       val actualColumns: List[ColumnIdentifier] = columnDefinitions.map(_.id)
       val invalidColumns: List[ColumnReference] = columns.filterNot(f => actualColumns.exists(_ == f.ref))

       if(invalidColumns.isEmpty)
         None
       else
         Some(invalidColumns.map(_.ref))

     case _ =>
       None
   }

   val v = for {
     cd <- columnDefinitions
     rule <- cd.rules
     invalidColumns = uniqueMultiCheck(rule)
     _ <- invalidColumns
   } yield s"""Column: ${cd.id.value}: Invalid cross reference ${invalidColumns.get.map(_.toString).mkString("$", ", $", "")}: at line: ${rule.pos.line}, column: ${rule.pos.column}"""

   if (v.isEmpty) None else Some(v.mkString(EOL))
 }

 protected def explicitColumnValid(columnDefinitions: List[ColumnDefinition]): Option[String] = {

   def invalidColumnNames(rule: Rule) = explicitColumnCheck(rule).getOrElse(List.empty[ColumnIdentifier])

   def checkAlternativeOption(rules: Option[List[Rule]]): Option[List[ColumnIdentifier]] = {
     rules.map{ _.foldLeft(List.empty[ColumnIdentifier]) {
         case (list, rule: Rule) => list ++ invalidColumnNames(rule)
       }
     }
   }


   def explicitColumnCheck(rule: Rule): Option[List[ColumnIdentifier]] = rule match {
     case IfRule(c, t, f) =>
       val cond = explicitColumnCheck(c)
       val cons = t.foldLeft(Some(List.empty[ColumnIdentifier])) { case (l, r) => Some((l ++ explicitColumnCheck(r)).flatten.toList)}
       val alt = checkAlternativeOption(f)
       Some((cond ++ cons ++ alt).flatten.toList)

     case AndRule(lf, rt) =>
       val left = explicitColumnCheck(lf)
       val right = explicitColumnCheck(rt)
       Some((left ++ right).flatten.toList)

     case OrRule(lf, rt) =>
       val left = explicitColumnCheck(lf)
       val right = explicitColumnCheck(rt)
       Some((left ++ right).flatten.toList)

     case ParenthesesRule(l) =>
       l.foldLeft(Some(List.empty[ColumnIdentifier])) { case (l, r) => Some((l ++ explicitColumnCheck(r)).flatten.toList)}

     case _ =>
       rule.explicitColumn match {
         case Some(columnRef) if (!columnDefinitions.map(_.id).contains(columnRef.ref)) =>
           Some(List(columnRef.ref))
         case _ =>
           None
       }
   }

   val result = for {
     cd <- columnDefinitions
     rule <- cd.rules
     errorColumn = explicitColumnCheck(rule)
     if (errorColumn.isDefined && errorColumn.get.length > 0)
   } yield s"""Column: ${cd.id}: Invalid explicit column ${errorColumn.get.mkString(", ")}: at line: ${rule.pos.line}, column: ${rule.pos.column}"""

   if (result.isEmpty) None else Some(result.mkString(EOL))
 }
}


object SchemaValidator {
  def apply(g: List[GlobalDirective], c: List[ColumnDefinition]): String = {
    val parser = new SchemaValidator()
    parser.validate(g,c)
  }
}
