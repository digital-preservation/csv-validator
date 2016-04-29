/**
 * Copyright (c) 2013, The National Archives <digitalpreservation@nationalarchives.gov.uk>
 * http://www.nationalarchives.gov.uk
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package uk.gov.nationalarchives.csv.validator.schema.v1_2

import java.nio.charset.Charset

import collection.JavaConverters._
import uk.gov.nationalarchives.csv.validator.metadata.Row
import uk.gov.nationalarchives.csv.validator.schema.{Schema, ArgProvider}
import java.net.{URLDecoder => JURLDecoder}
import java.net.{URLEncoder => JURLEncoder}


/**
 * Created by rhubner on 4/26/16.
 */

//case class NoExt(value: ArgProvider) extends ArgProvider


case class UrlDecode(value: ArgProvider, enconding: Option[ArgProvider]) extends ArgProvider {

  val DefaultEncoding = "UTF-8"

  override def referenceValue(columnIndex: Int, row: Row, schema: Schema): Option[String] = value.referenceValue(columnIndex, row, schema).map( value => {

    val codepage = enconding.flatMap(x => x.referenceValue(columnIndex, row, schema)).getOrElse(DefaultEncoding)

    JURLDecoder.decode(value,codepage)

  })

  override def toError: String = "urlDecode(" + value.toError + ")"
}