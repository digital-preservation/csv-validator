/*
 * Copyright (c) 2013, The National Archives <digitalpreservation@nationalarchives.gov.uk>
 * https://www.nationalarchives.gov.uk
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package uk.gov.nationalarchives.csv.validator.schema

import just.decver.DecVer
import just.semver.SemVer
import uk.gov.nationalarchives.csv.validator.schema.v1_0.{SchemaValidator => SchemaValidator1_0}
import uk.gov.nationalarchives.csv.validator.schema.v1_1.{SchemaValidator => SchemaValidator1_1}

/**
  * Utility object to validate schema consistency
  */
object SchemaValidator {
  /**
    * Validate the schema against certain rules depending on the schema Version
    * @param schema
    * @return
    */
  def validate(schema: Schema): String = {
    val g: List[GlobalDirective] = schema.globalDirectives
    val c: List[ColumnDefinition] = schema.columnDefinitions

    schema.version match {
        case "1.1" => SchemaValidator1_1(g,c)
        case _ => SchemaValidator1_0(g,c)
      }
  }

   def versionValid(version: String): Option[String] =  {
     val thisVersionGreater = for {
       thisVersion <- DecVer.parse(version)
       schemaVersion <- DecVer.parse(Schema.version)
     } yield thisVersion > schemaVersion

     thisVersionGreater match {
       case Left(_) => Some(s"Error parsing schema version $version")
       case Right(true) => Some(s"Invalid schema version. This version of the csv validator supports only ${Schema.version} and below.")
       case Right(false) => None
     }
  }
}



