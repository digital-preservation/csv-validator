/*
 * Copyright (c) 2013, The National Archives <digitalpreservation@nationalarchives.gov.uk>
 * https://www.nationalarchives.gov.uk
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package uk.gov.nationalarchives.csv.validator.ui

import swing._
import scala.swing.event.{ButtonClicked, Key, KeyPressed, MouseClicked, UIElementResized}
import swing.FileChooser.Result
import swing.GridBagPanel.Anchor
import java.beans.{PropertyChangeEvent, PropertyChangeListener}
import java.nio.file.Path
import scala.swing.Dialog.Message
import java.io.IOException
import javax.swing.JTextField
import scala.util.{Failure, Success, Try}

/**
 * Some simple helpers to ease
 * the use of scala.swing
 */
object ScalaSwingHelpers {
  private def handleSelectedFileOrDir(dialogAction: Result.Value, fileOrDirChooser: FileChooser, result: Path => Try[String], fileAction: String): Unit =
    dialogAction match {
      case Result.Approve =>
        result(fileOrDirChooser.selectedFile.toPath) match {
          case Failure(e) =>
            e.printStackTrace()
            Dialog.showMessage(fileOrDirChooser, s"${e.getClass.getName}: ${e.getMessage}", s"Unable to ${fileAction.toLowerCase()} file", Message.Error)
          case _ => ()
        }
      case _ => ()
    }

  /**
   * Opens a FileChooser and sets the path of the chosen file/dir as the text of a Text Component
   *
   * @param fileOrDirChooser FileChooser which is Used to open file/dir dialogs
   * @param output A text component which displays the absolute path of the chosen file
   * @param locateOver A component over which the FileChooser dialog should be located
   */
  def chooseFileOrDir(fileOrDirChooser: FileChooser, output: JTextField, locateOver: Component) : Unit =
    chooseFileOrDir(fileOrDirChooser, f => Try(output.setText(f.toAbsolutePath.toString)).map(_ => "Path updated"), locateOver)

  /**
   * Opens a FileChooser and sends the result to a function
   *
   * @param fileOrDirChooser FileChooser which is Used to open file/dir dialogs
   * @param result A function which takes the chosen file
   * @param locateOver A component over which the FileChooser dialog should be located
   * @param approveBtnText The text to appear on the approval button of the dialog box
   */
  def chooseFileOrDir(fileOrDirChooser: FileChooser, result: Path => Try[String], locateOver: Component, approveBtnText: String = "OK") : Unit = {
    val showDialogAction = fileOrDirChooser.showDialog(locateOver, approveBtnText)
    handleSelectedFileOrDir(showDialogAction, fileOrDirChooser, result, "get")
  }

  /**
   * Opens a FileChooser and sends the result to a function
   *
   * @param fileChooser FileChooser which is Used to open file dialogs
   * @param result A function which writes the report out
   * @param locateOver A component over which the FileChooser dialog should be located
   * @param approveBtnText The text to appear on the approval button of the dialog box
   */
  def saveFile(fileChooser: FileChooser, result: Path => Try[String], locateOver: Component, approveBtnText: String) : Unit = {
    val saveDialogAction = fileChooser.showSaveDialog(locateOver)
    handleSelectedFileOrDir(saveDialogAction, fileChooser, result, approveBtnText)
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
  case class Row(label: String, components: List[Component])
  val c = List()
  def addToTableDialog(owner: Window, title: String, rows: List[Row], result: Array[String] => Unit) : Unit = {

    val btnOk = new Button("OK")

    val optionLayout: GridBagPanel = new GridBagPanel {
      val c = new Constraints
      rows.zipWithIndex.map {
        case (row, colIdx) =>
          c.gridx = 0
          c.gridy = colIdx
          c.anchor = Anchor.LineStart
          layout(new Label(row.label + ":")) = c

          row.components.zipWithIndex.map {
            case (component, rowIdx) =>
              c.gridx = rowIdx + 1
              c.gridy = colIdx
              c.anchor = Anchor.LineStart
              layout(component) = c
          }
      }

      c.gridx = 0
      c.gridy = rows.size
      c.gridwidth = rows.size + 1
      c.anchor = Anchor.LineEnd
      layout(btnOk) = c
    }

    val dialog = new Dialog(owner)
    dialog.modal = true
    dialog.title = title
    dialog.contents = optionLayout
    dialog.setLocationRelativeTo(owner)

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

  def doNothingOnClick(): Reactions.Reaction = {
    case evt: ButtonClicked => ()
    case evt: MouseClicked => ()
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

  def PropertyChangeListener(f: PropertyChangeEvent => Unit): PropertyChangeListener = new PropertyChangeListener {
    def propertyChange(e: PropertyChangeEvent) : Unit = {
      f(e)
    }
  }

  def onResize(action: => Unit) : Reactions.Reaction = {
    case evt: UIElementResized => action
  }
}
