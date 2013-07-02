package uk.gov.tna.dri.schema
/*
 * Copyright (c) 2013, The National Archives digitalpreservation@nationalarchives.gov.uk
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
/*
 * Copyright (c) 2013, The National Archives digitalpreservation@nationalarchives.gov.uk
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

import scalax.file.{PathSet, Path}
import scalaz.Scalaz._
import scalaz.{Success => SuccessZ, Failure => FailureZ}
import java.io.{BufferedInputStream, FileInputStream, File}
import java.security.MessageDigest
import uk.gov.tna.dri.metadata.Row
import util.Try
import annotation.tailrec
import java.net.URI
import org.joda.time.{Interval, LocalTime, DateTime}
import org.joda.time.format.DateTimeFormat


trait DateParser {
  def parse(dateStr: String): Try[DateTime]
}

object IsoDateParser extends DateParser {
  def parse(dateStr: String): Try[DateTime] = Try(DateTime.parse(dateStr))
}

object UkDateParser extends DateParser {
  val format = DateTimeFormat.forPattern(UkDateFormat)
  def parse(dateStr: String): Try[DateTime] = Try(format.parseDateTime(dateStr))
}

object TimeParser extends DateParser {
  def parse(dateStr: String) = Try(LocalTime.parse(dateStr).toDateTimeToday)
}

abstract class DateRangeRule(name: String, dateRegex: String, dateParser: DateParser) extends Rule(name) {
  import dateParser.parse
  val from: String
  val to: String
  lazy val fromDate = parse(from)
  lazy val toDate = parse(to)

  def valid(cellValue: String, columnDefinition: ColumnDefinition, columnIndex: Int, row: Row, schema: Schema): Boolean = {
    cellValue matches dateRegex match {
      case true => {
        val inRange = for ( frmDt <- fromDate; toDt <- toDate; cellDt <- parse(cellValue)) yield {
          val interval = new Interval(frmDt,toDt.plusMillis(1))
          interval.contains(cellDt)
        }

        inRange.getOrElse(false)
      }

      case _ => false
    }
  }

  override def toError = s"""$ruleName("$from, $to")"""
}

abstract class PatternRule(name: String, pattern: String) extends Rule(name) {
  def valid(cellValue: String, columnDefinition: ColumnDefinition, columnIndex: Int, row: Row, schema: Schema): Boolean = cellValue matches pattern
}

abstract class DateRule(name: String, dateRegex: String, dateParser: DateParser) extends PatternRule(name, dateRegex) {
  override def valid(cellValue: String, columnDefinition: ColumnDefinition, columnIndex: Int, row: Row, schema: Schema): Boolean = {
    super.valid(cellValue, columnDefinition, columnIndex, row, schema) match {
      case true => dateParser.parse(cellValue).isSuccess
      case _ => false
    }
  }
}

case class XsdDateTimeRule() extends DateRule("xDateTime", XsdDateTimeRegex, IsoDateParser)

case class XsdDateTimeRangeRule(from: String, to: String) extends DateRangeRule("xDateTime", XsdDateTimeRegex, IsoDateParser)

case class XsdDateRule() extends DateRule("xDate", XsdDateRegex, IsoDateParser)

case class XsdDateRangeRule(from: String, to: String) extends DateRangeRule("xDate",  XsdDateRegex, IsoDateParser)

case class UkDateRule() extends DateRule("ukDate", UkDateRegex, UkDateParser)

case class UkDateRangeRule(from: String, to: String) extends DateRangeRule("ukDate", UkDateRegex, UkDateParser)

case class XsdTimeRule() extends DateRule("xTime", XsdTimeRegex, TimeParser)

case class XsdTimeRangeRule(from: String, to: String) extends DateRangeRule("xTime", XsdTimeRegex, TimeParser)

case class PartUkDateRule() extends PatternRule("partUkDate", PartUkDateRegex)

