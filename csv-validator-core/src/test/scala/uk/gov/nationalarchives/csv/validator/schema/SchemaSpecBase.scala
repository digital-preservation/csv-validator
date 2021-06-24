/**
 * Copyright (c) 2013, The National Archives <digitalpreservation@nationalarchives.gov.uk>
 * https://www.nationalarchives.gov.uk
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package uk.gov.nationalarchives.csv.validator.schema

import org.specs2.mutable.Specification
import uk.gov.nationalarchives.csv.validator.schema.v1_0.NotEmptyRule


trait SchemaSpecBase extends Specification {

  object TestSchemaParser extends SchemaParser { val pathSubstitutions = List[(String,String)](); val enforceCaseSensitivePathChecks = false; val trace = false }

  def buildSchema1_0(globalDirective: GlobalDirective*)(columnDefinition: ColumnDefinition*) =
    Schema(globalDirective.toList, columnDefinition.toList, "1.0")

  def buildSchema1_1(globalDirective: GlobalDirective*)(columnDefinition: ColumnDefinition*) =
    Schema(globalDirective.toList, columnDefinition.toList, "1.1")

  def buildSchema1_2(globalDirective: GlobalDirective*)(columnDefinition: ColumnDefinition*) =
    Schema(globalDirective.toList, columnDefinition.toList, "1.2")

  def namedColumn(name: String) = ColumnDefinition(NamedColumnIdentifier(name))

  def nonEmptyColumn(name: String): ColumnDefinition =
    ColumnDefinition(NamedColumnIdentifier(name), List(NotEmptyRule()), List())

}
