/*
 * Copyright (c) 2013, The National Archives digitalpreservation@nationalarchives.gov.uk
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package uk.gov.tna.dri.csv.validator.ui

import swing.{Component, Panel}
import org.jdesktop.swingx.{JXTaskPane, JXTaskPaneContainer}

/**
 * scala.swing compatible implementation of JXTaskPaneContainer
 */
class TaskPaneContainer extends Panel {
  override lazy val peer = new JXTaskPaneContainer
  def add(taskPane: TaskPane) = peer.add(taskPane.peer)
}

/**
 * scala.swing compatible implementation of JXTaskPane
 */
class TaskPane(title: String) extends Panel {
  override lazy val peer = new JXTaskPane(title)
  def this() = this("")
  def this(title : String, collapsed : Boolean) = {
    this(title)
    this.collapsed = collapsed
  }
  def add(component: Component) = peer.add(component.peer)
  def collapsed = peer.isCollapsed
  def collapsed_= (c : Boolean) {
    peer.setCollapsed(c)
  }
}
