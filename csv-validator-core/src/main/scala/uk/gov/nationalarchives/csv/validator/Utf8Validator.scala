package uk.gov.nationalarchives.csv.validator

import java.nio.file.Path
import uk.gov.nationalarchives.csv.validator.Utf8Validator._

import java.io.{BufferedInputStream, File, FileInputStream}
import scala.annotation.tailrec

class Utf8Validator {

  private val DEFAULT_BUFFER_SIZE = 8192

  private val FOUR_BYTE_CHAR = 0xF0 // 11110xxx

  private val THREE_BYTE_CHAR = 0xE0 // 1110xxxx

  private val TWO_BYTE_CHAR = 0xC0 // 110xxxxx

  def validate(path: Path): List[Utf8ValidationError] = {
    val is = new BufferedInputStream(new FileInputStream(path.toString), DEFAULT_BUFFER_SIZE)

    @tailrec
    def readBytes(read: Int, multiByteLen: Int, multiBytesRemain: Int, errors: List[Utf8ValidationError]): (List[Utf8ValidationError], Int, Int, Int) = {
      val b = is.read()
      if (b == -1) {
        (errors, read + 1, multiByteLen, multiBytesRemain)
      } else {
        if (multiBytesRemain > 0) {
          if ((b >>> 6) != 2) {
            val error = s"Invalid UTF-8 sequence, byte ${multiByteLen - multiBytesRemain - 1} of $multiByteLen byte sequence."
            readBytes(read + 1, multiByteLen, multiBytesRemain - 1, Utf8ValidationError(read, error) :: errors)
          } else {
            readBytes(read + 1, multiByteLen, multiBytesRemain - 1, errors)
          }

        }
        else if ((b & 0x80) == 0) {
          readBytes(read + 1, multiByteLen, multiBytesRemain, errors)
        }
        else if ((b & FOUR_BYTE_CHAR) == FOUR_BYTE_CHAR) {
          //Four byte Sequence
          readBytes(read + 1, 4, 3, errors)
        }
        else if ((b & THREE_BYTE_CHAR) == THREE_BYTE_CHAR) {
          //Three byte Sequence
          readBytes(read + 1, 3, 2, errors)
        }
        else if ((b & TWO_BYTE_CHAR) == TWO_BYTE_CHAR) {
          //Two byte Sequence
          readBytes(read + 1, 1, 1, errors)
        }
        else readBytes(read + 1, 1, 1, Utf8ValidationError(read, "Invalid single byte UTF-8 character") :: errors)
      }
    }
    val (errors, read, multiByteLen, multiBytesRemain) = readBytes(0, 0, 0, Nil)
    if(multiBytesRemain > 0) {
      Utf8ValidationError(read, s"Invalid UTF-8 Sequence, expecting: $multiBytesRemain more bytes in $multiByteLen byte sequence. End of File!") :: errors
    } else {
      errors
    }
  }
}
object Utf8Validator {
  case class Utf8ValidationError(offset: Int, message: String)
  def apply() = new Utf8Validator()
}
