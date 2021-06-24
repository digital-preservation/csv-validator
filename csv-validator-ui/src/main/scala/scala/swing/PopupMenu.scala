/**
 * Copyright (c) 2013, The National Archives <digitalpreservation@nationalarchives.gov.uk>
 * https://www.nationalarchives.gov.uk
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package scala.swing

import javax.swing.JPopupMenu

/**
 * Implementation of JPopupMenu for scala.swing
 */
class PopupMenu(title0: String) extends SequentialContainer.Wrapper { self: PopupMenu =>
  lazy val peer: JPopupMenu = new JPopupMenu(title0)

  def this() = this(null)
}

/**
 * Some simple implicits to enable
 * easier integration of PopupMenu with the existing
 * scala.swing classes
 */
object PopupMenuImplicits {
  implicit def componentPopupMenu(component: Component): { def popupMenu(menu: PopupMenu) } = new {
    def popupMenu(menu: PopupMenu) {
      component.peer.setComponentPopupMenu(menu.peer)
    }
  }
}