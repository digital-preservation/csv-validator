/**
 * Copyright (c) 2013, The National Archives <digitalpreservation@nationalarchives.gov.uk>
 * http://www.nationalarchives.gov.uk
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package uk.gov.nationalarchives.csv.validator

import scalaz._, Scalaz._
import uk.gov.nationalarchives.csv.validator.schema._
import uk.gov.nationalarchives.csv.validator.metadata.Cell
import uk.gov.nationalarchives.csv.validator.metadata.Row
import uk.gov.nationalarchives.csv.validator.schema.Warning
import uk.gov.nationalarchives.csv.validator.schema.TotalColumns
import uk.gov.nationalarchives.csv.validator.schema.Optional
import scala.annotation.tailrec

trait AllErrorsMetaDataValidator extends MetaDataValidator {

  def validateRows(rows: Iterator[Row], schema: Schema): MetaDataValidation[Any] = {

    @tailrec
    def validateRows(results: List[MetaDataValidation[Any]] = List.empty[MetaDataValidation[Any]]) : List[MetaDataValidation[Any]] = {
      if(!rows.hasNext) {
        results.reverse
      } else {
        val row = rows.next()
        val result = validateRow(row, schema)
        validateRows(result :: results)
      }
    }

    val v = validateRows()
    v.sequence[MetaDataValidation, Any]
  }


  private def rules(row: Row, schema: Schema): MetaDataValidation[List[Any]] = {
    val cells: (Int) => Option[Cell] = row.cells.lift
    val v = for {(columnDefinition, columnIndex) <- schema.columnDefinitions.zipWithIndex} yield validateCell(columnIndex, cells, row, schema)
    v.sequence[MetaDataValidation, Any]
  }




}