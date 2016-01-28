/**
 * Copyright (c) 2013, The National Archives <digitalpreservation@nationalarchives.gov.uk>
 * http://www.nationalarchives.gov.uk
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package uk.gov.nationalarchives.csv

import java.nio.charset.Charset

import scala.sys

package object validator {

  /**
   * End-of-Line
   */
  val EOL = sys.props("line.separator")

  val ENCODING = Charset.forName("UTF-8")

  /**
   * Separator in file paths i.e. '\' or '/'
   */
  val FILE_SEPARATOR = sys.props("file.separator").head

  val WINDOWS_FILE_SEPARATOR = '\\'

  val UNIX_FILE_SEPARATOR = '/'

  val URI_PATH_SEPARATOR = '/'
}
