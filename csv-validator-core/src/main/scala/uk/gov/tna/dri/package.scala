package uk.gov.tna

package object dri {

  /**
   * End-of-Line
   */
  val EOL = sys.props("line.separator")

  /**
   * Separator in file paths i.e. '\' or '/'
   */
  val FILE_SEPARATOR = sys.props("file.separator")

  val WINDOWS_FILE_SEPARATOR = '\\'

  val UNIX_FILE_SEPARATOR = '/'
}
