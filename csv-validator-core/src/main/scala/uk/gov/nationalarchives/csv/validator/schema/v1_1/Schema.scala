/**
 * Copyright (c) 2013, The National Archives <digitalpreservation@nationalarchives.gov.uk>
 * https://www.nationalarchives.gov.uk
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package uk.gov.nationalarchives.csv.validator.schema.v1_1

import org.apache.commons.io.FilenameUtils
import uk.gov.nationalarchives.csv.validator.metadata.Row
import uk.gov.nationalarchives.csv.validator.schema._

case class NoExt(value: ArgProvider) extends ArgProvider {

  def referenceValue(columnIndex: Int, row: Row, schema: Schema): Option[String] = value.referenceValue(columnIndex, row, schema).map(FilenameUtils.removeExtension(_))

  def toError = "noExt(" + value.toError + ")"

}

case class Concat(args: ArgProvider*) extends  ArgProvider {

  def referenceValue(columnIndex: Int, row: Row, schema: Schema): Option[String] = concat(args.map(_.referenceValue(columnIndex, row, schema)))

  def concat(ms: Seq[Option[String]]): Option[String] =
    if (ms.forall(_.isEmpty))
      None
    else {
      Some(ms.foldLeft(""){case (acc,elem) => acc + elem.getOrElse("")})
    }

  def toError = "concat(" + args.map(_.toError).mkString(", ") + ")"
}

