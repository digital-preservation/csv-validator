/*
 * Copyright (c) 2013, The National Archives <digitalpreservation@nationalarchives.gov.uk>
 * https://www.nationalarchives.gov.uk
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package uk.gov.nationalarchives.csv.validator.ui

import scala.swing._
import org.jdesktop.swingx.{JXCollapsiblePane, JXFrame, JXTaskPane, JXTaskPaneContainer}
import javax.swing.JFrame
import scala.swing.event.{ActionEvent, ButtonClicked}
import uk.gov.nationalarchives.csv.validator.ui.SJXTaskPane.ViewStateChanged

/**
 * scala.swing compatible implementation of JXFrame
 */
class SJXFrame(gc: java.awt.GraphicsConfiguration = null) extends Frame(gc) {
  override lazy val peer: JFrame with InterfaceMixin2 = new JXFrame(gc) with InterfaceMixin2 with SuperMixin
}

/**
 * scala.swing compatible implementation of JXTaskPaneContainer
 */
class SJXTaskPaneContainer extends Panel {
  override lazy val peer = new JXTaskPaneContainer
  def add(taskPane: SJXTaskPane) = peer.add(taskPane.peer)
}

/**
 * scala.swing compatible implementation of JXTaskPane
 */
class SJXTaskPane(title: String) extends Panel with Publisher {
  override lazy val peer = new JXTaskPane(title)
  peer.addPropertyChangeListener("collapsed", ScalaSwingHelpers.PropertyChangeListener { e =>
    publish(ViewStateChanged(SJXTaskPane.this))
  })

  def this() = this("")
  def this(title : String, collapsed : Boolean) = {
    this(title)
    this.collapsed = collapsed
  }
  def add(component: Component) = peer.add(component.peer)
  def remove(component: Component) = peer.remove(component.peer)
  def collapsed = peer.isCollapsed
  def collapsed_= (c : Boolean) : Unit = {
    peer.setCollapsed(c)
  }
}

object SJXTaskPane {

  case class ViewStateChanged(override val source: SJXTaskPane) extends ActionEvent(source)

  def onViewStateChanged(action: => Unit): Reactions.Reaction = {
    case evt: ViewStateChanged =>
      action
  }
}