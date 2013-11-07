package uk.gov.tna.dri.csv

import scala.sys
import scala.sys
import scalaz._

package object validator {

  /**
   * End-of-Line
   */
  val EOL = sys.props("line.separator")

  /**
   * Separator in file paths i.e. '\' or '/'
   */
  val FILE_SEPARATOR = sys.props("file.separator").head

  val WINDOWS_FILE_SEPARATOR = '\\'

  val UNIX_FILE_SEPARATOR = '/'

  val URI_PATH_SEPARATOR = '/'
}
