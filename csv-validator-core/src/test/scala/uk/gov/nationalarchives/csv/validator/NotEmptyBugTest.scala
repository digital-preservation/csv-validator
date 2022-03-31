/*
 * Copyright (c) 2013, The National Archives <digitalpreservation@nationalarchives.gov.uk>
 * https://www.nationalarchives.gov.uk
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package uk.gov.nationalarchives.csv.validator

import java.io.{Reader, StringReader}

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import uk.gov.nationalarchives.csv.validator.schema.{Schema, _}

import scala.language.reflectiveCalls
import scalaz.{Failure, IList, Success}

@RunWith(classOf[JUnitRunner])
class NotEmptyBugTest extends Specification with TestResources {

  implicit def stringToStringReader(s: String): StringReader = new StringReader(s.replaceAll("\n\\s+", "\n"))

  implicit def stringToSchema(s: String): Schema = {
    val schemaParser = new SchemaParser() {
      val pathSubstitutions = List[(String,String)]()
      val enforceCaseSensitivePathChecks = false
      val trace = false

      override def parse(reader: Reader): ParseResult[Schema] = {
        super.parse(reader) match {
          case s@Success(schema: Schema, _) => s
          case NoSuccess(message, next) => throw new RuntimeException(message)
        }
      }
    }

    schemaParser.parse(s).get
  }

  object TestMetaDataValidator extends AllErrorsMetaDataValidator {
    val pathSubstitutions = List[(String,String)]();
    val trace = false
  }

  import TestMetaDataValidator._


  "Validation" should {

  "succeed for more than 1 notEmpty rule with different columns" in {
    val schema =
      """version 1.1
         @totalColumns 2
         file_name: notEmpty if($curated_file_name/notEmpty, is(noext($curated_file_name) ))
         curated_file_name: @optional
      """

    val metaData =
      """file_name,curated_file_name
         content,
      """
    validate(metaData, schema, None) must beLike { case Success(_) => ok }

  }

  "succeed for more than 1 empty rule with different columns" in {
    val schema =
      """version 1.1
         @totalColumns 2
         file_name: empty if($curated_file_name/empty, is(noext($curated_file_name)))
         curated_file_name: @optional
      """

    val metaData =
      """file_name,curated_file_name
         ,content
      """
    validate(metaData, schema, None) must beLike { case Success(_) => ok }
  }

  "succeed for more than 1 uuid4 rule with different columns" in {
    val schema =
      """version 1.1
         @totalColumns 2
         file_name: uuid4 if($curated_file_name/uuid4, is(noext($curated_file_name)))
         curated_file_name: @optional
      """

    val metaData =
      """file_name,curated_file_name
         8f60aab0-f66d-48d8-9382-f692b26b34dc,not-uuidv4-value
      """
    validate(metaData, schema, None) must beLike { case Success(_) => ok }
  }

  "succeed for more than 1 positiveInteger rule with different columns" in {
    val schema =
      """version 1.1
         @totalColumns 2
         file_name: positiveInteger if($curated_file_name/positiveInteger, is(noext($curated_file_name)))
         curated_file_name: @optional
      """

    val metaData =
      """file_name,curated_file_name
         111,-1224544
      """
    validate(metaData, schema, None) must beLike { case Success(_) => ok }
  }

  "succeed for more than 1 uri rule with different columns" in {
    val schema =
      """version 1.1
         @totalColumns 2
         file_name: uri if($curated_file_name/uri, is(noext($curated_file_name)))
         curated_file_name: @optional
      """

    val metaData =
      """file_name,curated_file_name
         http://www.root.cz/,not a URI
      """
    validate(metaData, schema, None) must beLike { case Success(_) => ok }
  }

  "succeed for more than 1 partUkDate rule with different columns" in {
    val schema =
      """version 1.1
         @totalColumns 2
         file_name: partUkDate if($curated_file_name/partUkDate, is(noext($curated_file_name)))
         curated_file_name: @optional
      """

    val metaData =
      """file_name,curated_file_name
         04/February/1981,not a date
      """
    validate(metaData, schema, None) must beLike { case Success(_) => ok }
  }

  "succeed for more than 1 lowerCase rule with different columns" in {
    val schema =
      """version 1.1
         @totalColumns 2
         file_name: lowerCase if($curated_file_name/lowerCase, is(noext($curated_file_name)))
         curated_file_name: @optional
      """

    val metaData =
      """file_name,curated_file_name
         lowercase text,CamelCaseText
      """
    validate(metaData, schema, None) must beLike { case Success(_) => ok }
  }

  "succeed for more than 1 upperCase rule with different columns" in {
    val schema =
      """version 1.1
         @totalColumns 2
         file_name: upperCase if($curated_file_name/upperCase, is(noext($curated_file_name)))
         curated_file_name: @optional
      """

    val metaData =
      """file_name,curated_file_name
         UPPERCASE,CamelCaseText
      """
    validate(metaData, schema, None) must beLike { case Success(_) => ok }
  }

  "succeed for more than 1 identical rule with different columns" in {
    val schema =
      """version 1.1
         @totalColumns 2
         file_name: identical or lowerCase
         curated_file_name: identical
      """

    val metaData =
      """file_name,curated_file_name
        ble,sameValue
        elb,sameValue
      """
    val result = validate(metaData, schema, None)

    result must beLike { case Success(_) => ok }
  }
}
}