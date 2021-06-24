/**
 * Copyright (c) 2013, The National Archives <digitalpreservation@nationalarchives.gov.uk>
 * https://www.nationalarchives.gov.uk
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package uk.gov.nationalarchives.csv.validator.ui

import scala.language.implicitConversions
import net.java.dev.designgridlayout.{ISpannableGridRow, IRowCreator, IGridRow, IRow}
import swing.{Label, Component}

/**
 * Some simple Implicits to ease
 * working with DesignGridLayout
 * from scala.swing
 */
object DesignGridImplicits {

  implicit def iRowAddComponent(iRow: IRow): { def add(component: Component) : IRow } = new {
    def add(component: Component) : IRow = iRow.add(component.peer)
  }

  implicit def iGridRowAddComponentSpan(iGridRow: IGridRow): { def add(component: Component, gridSpan : Int) : IGridRow } = new {
    def add(component: Component, gridSpan: Int) : IGridRow = iGridRow.add(component.peer, gridSpan)
  }

  implicit def iRowCreatorGridLabel(iRowCreator: IRowCreator): { def grid(label: Label) : ISpannableGridRow } = new {
    def grid(label: Label) : ISpannableGridRow = iRowCreator.grid(label.peer)
  }
}
