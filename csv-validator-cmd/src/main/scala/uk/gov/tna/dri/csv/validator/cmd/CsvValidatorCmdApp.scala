/*
 * Copyright (c) 2013, The National Archives digitalpreservation@nationalarchives.gov.uk
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package uk.gov.tna.dri.csv.validator.cmd

import scalaz.{Success => SuccessZ, Failure => FailureZ, _}
import Scalaz._
import scala.App
import uk.gov.tna.dri.csv.validator._
import uk.gov.tna.dri.csv.validator.api.CsvValidator.createValidator
import uk.gov.tna.dri.csv.validator.Util._
import scalax.file.Path

object  SystemExits {
  val ValidCsv = 0
  val IncorrectArguments = 1
  val InvalidSchema = 2
  val InvalidCsv = 3
}

object CsvValidatorCmdApp extends App {

  val (exitMsg, exitCode) = run(args)
  println(exitMsg)
  System.exit(exitCode)

  def run(args: Array[String]): (String,Int) = {

    val result = for {
      ((metaDataFile, schemaFile), remainderArgs) <- findFiles(args.toList)
      (failFast, remainderArgs1) <- findFailFast(remainderArgs)
      (pathSubstitutionsList, remainderArgs2) <- findPaths(remainderArgs1)
      (_,_) <- unknownParams(remainderArgs2)
    } yield  {
      processMetaData(metaDataFile, schemaFile, failFast, pathSubstitutionsList)
    }

    result match {
      case Right(r) => (r._1, r._2)
      case Left(errMsg) => (errMsg, SystemExits.IncorrectArguments)
    }
  }

  def processMetaData(metaDataFile: Path, schemaFile: Path, failFast: Boolean, pathSubstitutionsList: List[(String,String)] ): (String,Int) = {
    val validator = createValidator(failFast, pathSubstitutionsList)
    validator.parseSchema(schemaFile) match {
      case FailureZ(errors) => (prettyPrint(errors), SystemExits.InvalidSchema)
      case SuccessZ(schema) =>
        validator.validate(metaDataFile, schema) match {
          case FailureZ(errors) => (prettyPrint(errors), SystemExits.InvalidCsv)
          case SuccessZ(_) => ("PASS", SystemExits.ValidCsv)
        }
    }
  }

  def findFailFast(args: List[String]): Either[String, (Boolean, List[String])] = {
    val (flags, remainder) = args.partition( a => a.startsWith("--fail-fast") || a == "-f")
    Right((!flags.isEmpty, remainder ))
  }


  def findFiles(args: List[String]): Either[String, ((Path,Path), List[String])] = {
    if ( args.length < 2)   Left(usage)
    else {
      val files = args.takeRight(2).map(Path.fromString(_))
      checkFilesReadable(files) match {
        case FailureZ(errors) => Left(prettyPrint(errors))
        case SuccessZ(_) =>
          val (metaDataFile: Path, schemaFile: Path) = (files(0),files(1))
          Right((metaDataFile, schemaFile), args.dropRight(2))
      }
    }
  }

  def findPaths(argsList: List[String]): Either[String, (List[(String,String)], List[String])] = {
    def filterPaths(argsWithIndex: List[(String,Int)]) = argsWithIndex.sliding(3).filter{  i => i(0)._1 == "--path"}.map( x => (x(1), x(2))).toList
    def removePathArgs(argsWithIndex: List[(String,Int)], path:List[((String,Int),(String,Int))]) = argsWithIndex.filter{ i =>  path.filter{pf => pf._1 == i || pf._2 == i}.isEmpty }

    if ( argsList.isEmpty ) Right(List.empty,argsList)
    else if (argsList.length < 3 ) { if( argsList.contains("--path")) Left("Missing param to --path" + EOL + usage) else Right(List.empty,argsList) }
    else {
      val argsWithIndex = argsList.zipWithIndex
      val path = filterPaths(argsWithIndex)
      val remaining = removePathArgs(argsWithIndex, path)
      Right(path.map( i => (i._1._1, i._2._1) ), remaining.unzip._1.filterNot(_ == "--path"))
    }
  }

  private def unknownParams(args: List[String]): Either[String, (List[(String,String)], List[String])] =
    if (!args.isEmpty) Left(s"Unknown parameter ${args.mkString(", ")}")
    else Right(List.empty, args)


  private def usage = """Usage: validate [--fail-fast] [--path <from> <to>]* <meta-data file path> <uk.gov.tna.dri.csv.validator.schema file path>"""

  private def prettyPrint(l: NonEmptyList[FailMessage]): String = l.list.map{i =>
    i match {
      case WarningMessage(err) => "Warning: " + err
      case ErrorMessage(err) =>   "Error:   " + err
      case SchemaMessage(err) =>  err
    }
  }.mkString(sys.props("line.separator"))
}