/**
 * Copyright (c) 2013, The National Archives <digitalpreservation@nationalarchives.gov.uk>
 * https://www.nationalarchives.gov.uk
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package uk.gov.nationalarchives.csv

import java.nio.charset.StandardCharsets

import scala.sys

package object validator {

  /**
   * End-of-Line
   */
  val EOL = sys.props("line.separator")

  val ENCODING = StandardCharsets.UTF_8

  /**
   * Separator in file paths i.e. '\' or '/'
   */
  val FILE_SEPARATOR = sys.props("file.separator").head

  val WINDOWS_FILE_SEPARATOR = '\\'

  val UNIX_FILE_SEPARATOR = '/'

  val URI_PATH_SEPARATOR = '/'

  val CSV_RFC1480_SEPARATOR: Char = ','

  val CSV_RFC1480_QUOTE_CHARACTER: Char = '"'

  val CSV_RFC1480_QUOTE_ESCAPE_CHARACTER: Char = '"'

  val CSV_RFC1480_LINE_SEPARATOR = Array('\r', '\n')


}
