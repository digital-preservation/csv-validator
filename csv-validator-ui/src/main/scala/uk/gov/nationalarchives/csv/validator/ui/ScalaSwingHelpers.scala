/**
 * Copyright (c) 2013, The National Archives <digitalpreservation@nationalarchives.gov.uk>
 * https://www.nationalarchives.gov.uk
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package uk.gov.nationalarchives.csv.validator.ui

import swing._
import scala.swing.event.{ButtonClicked, Key, KeyPressed, MouseClicked}
import swing.FileChooser.Result
import swing.GridBagPanel.Anchor
import java.beans.{PropertyChangeEvent, PropertyChangeListener}
import java.nio.file.Path
import scala.swing.Dialog.Message

/**
 * Some simple helpers to ease
 * the use of scala.swing
 */
object ScalaSwingHelpers {

  /**
   * Opens a FileChooser and sets the path of the chosen file as the text of a Text Component
   *
   * @param fileChooser
   * @param output A text component which displays the absolute path of the chosen file
   * @param locateOver A component over which the FileChooser dialog should be located
   */
  def chooseFile(fileChooser: FileChooser, output: TextComponent, locateOver: Component) : Unit = {
    chooseFile(fileChooser, {f => output.text = f.toAbsolutePath.toString; None}, locateOver)
  }

  /**
   * Opens a FileChooser and sends the result to a function
   *
   * @param fileChooser
   * @param result A function which takes the chosen file
   * @param locateOver A component over which the FileChooser dialog should be located
   */
  def chooseFile(fileChooser: FileChooser, result: Path => Option[IOException], locateOver: Component) : Unit = {
    fileChooser.showSaveDialog(locateOver) match {
      case Result.Approve =>
        result(fileChooser.selectedFile.toPath) match {
          case Some(ioe) =>
            ioe.printStackTrace()
            Dialog.showMessage(fileChooser, s"${ioe.getClass.getName}: ${ioe.getMessage}", "Unable to Save file", Message.Error)
          case None =>
        }
      case Result.Cancel =>
    }
  }

  /**
   * Given a Table we generate a simple key-value Dialog for adding a row to the dialog
   * we assume that the data type of each cell is String
   *
   * @param owner The window which owns this Dialog
   * @param title The title of the Dialog box
   * @param table The table to create a dialog for
   * @param result A function which takes a row as the result of the dialog box
   */
  def addToTableDialog(owner: Window, title: String, table: Table, result: Array[String] => Unit) : Unit = {

    val btnOk = new Button("Ok")

    val optionLayout = new GridBagPanel {
      val c = new Constraints

      for(colIdx <- (0 to table.model.getColumnCount - 1)) {
        c.gridx = 0
        c.gridy = colIdx
        c.anchor = Anchor.LineStart
        layout(new Label(table.model.getColumnName(colIdx) + ":")) = c

        c.gridx = 1
        c.gridy = colIdx
        c.anchor = Anchor.LineStart
        layout(new TextField(30)) = c
      }

      c.gridx = 0
      c.gridy = table.model.getColumnCount
      c.gridwidth = 2
      c.anchor = Anchor.LineEnd
      layout(btnOk) = c
    }

    val dialog = new Dialog(owner)
    dialog.modal = true
    dialog.title = title
    dialog.contents = optionLayout

    btnOk.reactions += onClick(result({
      val textValues = for{
        component <- optionLayout.contents
        if component.isInstanceOf[TextField]
      } yield component.asInstanceOf[TextField].text
      textValues.toArray}
    ))
    btnOk.reactions += onClick(dialog.close())

    dialog.visible = true
  }

  /**
   * Execute an Action when a Button is Clicked
   *
   * @param action The action function to invoke
   */
  def onClick(action: => Unit) : Reactions.Reaction = {
    case evt: ButtonClicked =>
      action

    case evt: MouseClicked =>
      action
  }

  /**
   * Execute an Action when a specific Key is pressed
   *
   * @param key The key which has to be pressed to invoke the action
   * @param action The action function to invoke
   */
  def onKeyPressed(key : Key.Value)(action: => Unit) : Reactions.Reaction = {
    case KeyPressed(_, _, key, _) =>
      action
  }

  def PropertyChangeListener(f: PropertyChangeEvent => Unit) = new PropertyChangeListener {
    def propertyChange(e: PropertyChangeEvent) : Unit = {
      f(e)
    }
  }
}
