/**
 * Copyright (c) 2013, The National Archives <digitalpreservation@nationalarchives.gov.uk>
 * http://www.nationalarchives.gov.uk
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package uk.gov.nationalarchives.csv.validator

import scalax.file.Path
import scalaz._
import Scalaz._


object Util {

  type AppValidation[S] = ValidationNel[FailMessage, S]

  def checkFilesReadable(files: List[Path]) = files.map(fileReadable).sequence[AppValidation, FailMessage]

  def fileReadable(file: Path): AppValidation[FailMessage] = if (file.exists && file.canRead) SchemaMessage(file.path).successNel[FailMessage] else fileNotReadableMessage(file).failNel[FailMessage]

  def fileNotReadableMessage(file: Path) = SchemaMessage("Unable to read file : " + file.path)
}
