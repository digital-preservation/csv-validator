package uk.gov.tna.dri.validator

import java.io.{FileReader, File}
import uk.gov.tna.dri.schema.{Schema, SchemaParser}
import scalaz.{Success => SuccessZ, Failure => FailureZ, _}
import Scalaz._
import scala.App

object  SystemExits {
  val ValidCsv = 0
  val IncorrectArguments = 1
  val InvalidSchema = 2
  val InvalidCsv = 3
}

object MetaDataValidatorCommandLineApp extends App {
  type AppValidation[S] = ValidationNEL[String, S]

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

  private def createValidator(failFast: Boolean, pathSubstitutionsList: List[(String,String)]) =
    if (failFast) new MetaDataValidatorApp with FailFastMetaDataValidator { val pathSubstitutions = pathSubstitutionsList }
    else new MetaDataValidatorApp with AllErrorsMetaDataValidator { val pathSubstitutions = pathSubstitutionsList }


  private def processMetaData(metaDataFile: String, schemaFile: String, failFast: Boolean, pathSubstitutionsList: List[(String,String)] ) = {
    val validator = createValidator (failFast, pathSubstitutionsList)
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


  def findFiles(args: List[String]): Either[String, ((String,String), List[String])] = {
    if ( args.length < 2)   Left(usage)
    else {
      val files = args.takeRight(2)
      checkFilesReadable(files) match {
        case FailureZ(errors) => Left(prettyPrint(errors))
        case SuccessZ(_) =>
          val (metaDataFile: String, schemaFile: String) = (files(0),files(1))
          Right((metaDataFile, schemaFile), args.dropRight(2))
      }
    }
  }

  def findPaths(args: List[String]): Either[String, (List[(String,String)], List[String])] =
    if ( args.isEmpty ) Right(List.empty,args)
    else if (args.length < 3 ) { if( args.contains("--path")) Left("Missing param to --path\n" + usage) else Right(List.empty,args) }
    else {
      val argsWithIndex = args.zipWithIndex
      val path = argsWithIndex.sliding(3).filter{  i => i(0)._1 == "--path"}.map( x => (x(1), x(2))).toList
      val remain = argsWithIndex.filter{ i =>  path.filter{pf => pf._1 == i || pf._2 == i}.isEmpty }
      Right(path.map( i => (i._1._1, i._2._1) ),remain.filterNot(_._1 == "--path").map(_._1))
    }


  def unknownParams(args: List[String]): Either[String, (List[(String,String)], List[String])] =
    if (!args.isEmpty) Left(s"Unknown parameter ${args.mkString(", ")}")
    else Right(List.empty, args)


  private def usage = """Usage: validate [--fail-fast] [--path <from> <to>]* <meta-data file path> <schema file path>"""

  private def checkFilesReadable(fileArgs: List[String]) = fileArgs.map(fileReadable).sequence[AppValidation, String]

  private def fileReadable(filePath: String): AppValidation[String] = if (new File(filePath).canRead) filePath.successNel[String] else fileNotReadableMessage(filePath).failNel[String]

  private def fileNotReadableMessage(filePath: String) = "Unable to read file : " + filePath

  private def prettyPrint(l: NonEmptyList[String]): String = l.list.mkString(sys.props("line.separator"))
}

trait MetaDataValidatorApp extends SchemaParser {
  this: MetaDataValidator =>

  def validate(metaDataFile: String, schema: Schema): MetaDataValidation[Any] = {
     validate(new FileReader(metaDataFile), schema)
  }

  def parseSchema(schemaFilePath: String): ValidationNEL[String, Schema] = parseAndValidate(new FileReader(schemaFilePath))
}