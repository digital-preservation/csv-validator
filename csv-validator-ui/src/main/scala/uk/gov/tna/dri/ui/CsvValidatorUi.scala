/*
 * Copyright (c) 2013, The National Archives digitalpreservation@nationalarchives.gov.uk
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package uk.gov.tna.dri.ui

import scala.swing._
import javax.swing._
import net.java.dev.designgridlayout._
import scala.swing.FileChooser.Result
import java.io.{File, PrintWriter}
import table.DefaultTableModel
import uk.gov.tna.dri.validator.MetaDataValidatorCommandLineApp
import org.jdesktop.swingx.{JXTaskPane, JXTaskPaneContainer}
import swing.event.{Key, KeyPressed, ButtonClicked}
import swing.GridBagPanel.Anchor
import uk.gov.tna.dri.ui.DesignGridImplicits._
import uk.gov.tna.dri.ui.PopupMenuImplicits._

object CsvValidatorUi extends SimpleSwingApplication {

  override def startup(args: Array[String]) {
    try {
      UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName)
    } catch {
      case ue : UnsupportedLookAndFeelException =>
        JOptionPane.showMessageDialog(this.top.peer, "Unable to set native look and feel: " + ue.getMessage, "Look and Feel Warning", JOptionPane.WARNING_MESSAGE)
    }
    super.startup(args)
  }

  def chooseFile(fileChooser: FileChooser, output: TextComponent, locateOver: Component) {
    chooseFile(fileChooser, f => output.text = f.getAbsolutePath, locateOver)
  }

  def chooseFile(fileChooser: FileChooser, result: File => Unit, locateOver: Component) {
    fileChooser.showOpenDialog(locateOver) match {
      case Result.Approve =>
        result(fileChooser.selectedFile)
      case Result.Cancel =>
    }
  }

  def addToTableDialog(title: String, headers: Array[String], table: Table, result: Array[String] => Unit) {

    val btnOk = new Button("Ok")

    val optionLayout = new GridBagPanel {
       val c = new Constraints

       for(i <- (0 to headers.length - 1)) {
         c.gridx = 0
         c.gridy = i
         c.anchor = Anchor.LineStart
         layout(new Label(headers(i) + ":")) = c

         c.gridx = 1
         c.gridy = i
         c.anchor = Anchor.LineStart
         layout(new TextField(30)) = c
       }

       c.gridx = 0
       c.gridy = headers.length
       c.gridwidth = 2
       c.anchor = Anchor.LineEnd
       layout(btnOk) = c
    }

    val dialog = new Dialog(this.top)
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
    btnOk.reactions += onClick(dialog.close)

    dialog.visible = true
  }

  def onClick(action: => Unit) : Reactions.Reaction = {
    case _: ButtonClicked =>
      action
  }

  def onKeyPressed(key : Key.Value)(action: => Unit) : Reactions.Reaction = {
    case KeyPressed(_, _, key, _) =>
      action
  }

  def validate(csvFilePath: String, csvSchemaFilePath: String, failOnFirstError: Boolean, pathSubstitutions: List[(String, String)], output: TextComponent) {
    output.text = MetaDataValidatorCommandLineApp.processMetaData(csvFilePath, csvSchemaFilePath, failOnFirstError, pathSubstitutions)._1
  }

  def saveToFile(s: String, f: File) {
    val printWriter = new PrintWriter(f)
    try {
      printWriter.write(s)
    } finally {
      printWriter.close
    }
  }

  def content : Panel = {

    val panel = new Panel{}
    val layout = new DesignGridLayout(panel.peer)

    val lblCsvFile = new Label("CSV file:")
    val txtCsvFile = new TextField(30)
    val csvFileChooser = new FileChooser
    val btnChooseCsvFile = new Button("...")
    btnChooseCsvFile.reactions += onClick(chooseFile(csvFileChooser, txtCsvFile, btnChooseCsvFile))

    val lblCsvSchemaFile = new Label("CSV Schema file:")
    val txtCsvSchemaFile = new TextField(30)
    val csvSchemaFileChooser = new FileChooser
    val btnChooseCsvSchemaFile = new Button("...")
    btnChooseCsvSchemaFile.reactions += onClick(chooseFile(csvSchemaFileChooser, txtCsvSchemaFile, btnChooseCsvSchemaFile))

    val taskPaneContainer = new TaskPaneContainer
    val settingsGroup = new TaskPane("Settings", true)
    val cbFailOnFirstError = new CheckBox("Fail on first error?")
    val lblPathSubstitutions = new Label("Path Substitutions")
    val headers = Array("From", "To")

    val tblPathSubstitutions = new Table(0, 2) {
      preferredViewportSize = new Dimension(500, 70)
      model = new DefaultTableModel(headers.asInstanceOf[Array[Object]], 0)

      def addRow(rowData: Array[String]) {
        model.asInstanceOf[DefaultTableModel].addRow(rowData.asInstanceOf[Array[AnyRef]])
      }

      def removeSelectedRow() {
        model.asInstanceOf[DefaultTableModel].removeRow(peer.getSelectedRow)
      }

      def pathSubstitutions : List[(String, String)] = {
        for(rowIdx <- (0 to model.getRowCount - 1)) yield (model.getValueAt(rowIdx, 0).asInstanceOf[String], model.getValueAt(rowIdx, 1).asInstanceOf[String])
      }.toList
    }

    val popupMenu = new PopupMenu
    val miRemove = new MenuItem("Remove Path Substitution")
    miRemove.reactions += onClick(tblPathSubstitutions.removeSelectedRow)
    popupMenu.contents += miRemove
    tblPathSubstitutions.popupMenu(popupMenu)

    val spTblPathSubstitutions = new ScrollPane(tblPathSubstitutions)
    val btnAddPathSubstitution = new Button("Add Path Substitution...")
    btnAddPathSubstitution.reactions += onClick(addToTableDialog("Add Path Substitution...", headers, tblPathSubstitutions, tblPathSubstitutions.addRow))

    val settingsGroupLayout = new GridBagPanel {
      val c = new Constraints
      c.gridx = 0
      c.gridy = 0
      layout(cbFailOnFirstError) = c

      c.gridx = 0
      c.gridy = 1
      c.insets = new Insets(0,10,0,0)
      layout(lblPathSubstitutions) = c

      c.gridx = 0
      c.gridy = 2
      c.gridwidth = 2
      layout(spTblPathSubstitutions) = c

      c.gridx = 1
      c.gridy = 3
      c.anchor = Anchor.LastLineEnd
      layout(btnAddPathSubstitution) = c
    }
    settingsGroup.add(settingsGroupLayout)
    taskPaneContainer.add(settingsGroup)

    val separator1 = new Separator

    val scrollPane = new ScrollPane
    val txtArReport = new TextArea(12,30)
    scrollPane.viewportView = txtArReport


    val btnValidate = new Button("Validate")
    btnValidate.reactions += onClick(validate(txtCsvFile.text, txtCsvSchemaFile.text, cbFailOnFirstError.selected, tblPathSubstitutions.pathSubstitutions, txtArReport))

    val separator2 = new Separator

    val btnClose = new Button("Close")
    btnClose.reactions += onClick(quit)

    val reportFileChooser = new FileChooser
    val btnSave = new Button("Save")
    btnSave.reactions += onClick(chooseFile(reportFileChooser, saveToFile(txtArReport.text, _), btnSave))

    layout.row.grid(lblCsvFile).add(txtCsvFile, 5).add(btnChooseCsvFile)
    layout.row.grid(lblCsvSchemaFile).add(txtCsvSchemaFile, 5).add(btnChooseCsvSchemaFile)

    layout.row.center.fill.add(taskPaneContainer)

    layout.row.center.fill.add(separator1)
    layout.row.center.fill.add(scrollPane)
    layout.row.center.fill.add(separator2)

    layout.row.right.withOwnRowWidth.add(btnSave).add(btnClose)

    panel
  }

  def top = new MainFrame {
    title = "CSV Validator"
    contents = content
  }
}
