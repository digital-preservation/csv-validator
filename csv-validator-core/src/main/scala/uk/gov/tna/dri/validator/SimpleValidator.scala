package uk.gov.tna.dri.validator

import scalaz._
import Scalaz._
import util.Try
import util.{Success => TSuccess}
import util.{Failure => TFailure}
import com.sun.xml.internal.fastinfoset.algorithm.BooleanEncodingAlgorithm

trait SimpleValidator {

  def int(str: String) : ValidationNEL[String, Boolean] = if (Try(str.toInt).isSuccess) true.successNel[String] else "Invalid int".failNel[Boolean]

  def strLengthTwo(str: String): ValidationNEL[String, Boolean] = if (str.length == 2) true.successNel[String] else "String length not 2".failNel[Boolean]

  def intLengthTwo(str: String) = {
    val intVal = int(str)
    val lengTwo = strLengthTwo(str)
    (intVal |@| lengTwo) tupled
  }
}
