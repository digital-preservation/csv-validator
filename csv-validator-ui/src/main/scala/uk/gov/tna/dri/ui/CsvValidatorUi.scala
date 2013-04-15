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
import scala.swing.event.ButtonClicked
import scala.swing.FileChooser.Result
import java.io.{File, PrintWriter}
import uk.gov.tna.dri.validator.MetaDataValidatorCommandLineApp
import org.jdesktop.swingx.{JXTaskPane, JXTaskPaneContainer}
import swing.event.ButtonClicked

object CsvValidatorUi extends SimpleSwingApplication {

  override def startup(args: Array[String]) {
    try {
      UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName())
    } catch {
      case ulafe : UnsupportedLookAndFeelException =>
        JOptionPane.showMessageDialog(this.top.peer, "Unable to set native look and feel: " + ulafe.getMessage, "Look and Feel Warning", JOptionPane.WARNING_MESSAGE)
    }
    super.startup(args)
  }

  implicit def iRowAddComponent(iRow: IRow): { def add(component: Component) : IRow } = new {
    def add(component: Component) : IRow = iRow.add(component.peer)
  }

  implicit def iGridRowAddComponentSpan(iGridRow: IGridRow): { def add(component: Component, gridSpan : Int) : IGridRow } = new {
    def add(component: Component, gridSpan: Int) : IGridRow = iGridRow.add(component.peer, gridSpan)
  }

  implicit def iRowCreatorGridLabel(iRowCreator: IRowCreator): { def grid(label: Label) : ISpannableGridRow } = new {
    def grid(label: Label) : ISpannableGridRow = iRowCreator.grid(label.peer)
  }

  def chooseFile(fileChooser: FileChooser, output: TextComponent, locateOver: Component) : Reactions.Reaction = chooseFile(fileChooser, f => output.text = f.getAbsolutePath, locateOver)

  def chooseFile(fileChooser: FileChooser, result: File => Unit, locateOver: Component) : Reactions.Reaction = {
    case _ : ButtonClicked =>
      fileChooser.showOpenDialog(locateOver) match {
        case Result.Approve =>
          result(fileChooser.selectedFile)
        case Result.Cancel =>
      }
  }

  def onClick(action : => Unit) : Reactions.Reaction = {
    case _: ButtonClicked =>
      action
  }

  def validate(csvFilePath: String, csvSchemaFilePath: String, failOnFirstError: Boolean, output: TextComponent) = {
    output.text = MetaDataValidatorCommandLineApp.processMetaData(csvFilePath, csvSchemaFilePath, failOnFirstError, List.empty)._1
  }

  def saveToFile(s: String, f: File) = {
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
    btnChooseCsvFile.reactions += chooseFile(csvFileChooser, txtCsvFile, btnChooseCsvFile)

    val lblCsvSchemaFile = new Label("CSV Schema file:")
    val txtCsvSchemaFile = new TextField(30)
    val csvSchemaFileChooser = new FileChooser
    val btnChooseCsvSchemaFile = new Button("...")
    btnChooseCsvSchemaFile.reactions += chooseFile(csvSchemaFileChooser, txtCsvSchemaFile, btnChooseCsvSchemaFile)

    val settingsPane = new JXTaskPaneContainer
    val settingsGroup = new JXTaskPane("Settings")
    settingsGroup.add(new JLabel("hello1"))
    settingsGroup.add(new JButton("hello2"))
    settingsPane.add(settingsGroup)

    val separator1 = new Separator

    val scrollPane = new ScrollPane
    val txtArReport = new TextArea(12,30)
    scrollPane.viewportView = txtArReport

    val cbFailOnFirstError = new CheckBox("Fail on first error?")
    val btnValidate = new Button("Validate")
    btnValidate.reactions += onClick(validate(txtCsvFile.text, txtCsvSchemaFile.text, cbFailOnFirstError.selected, txtArReport))

    val separator2 = new Separator

    val btnClose = new Button("Close")
    btnClose.reactions += onClick(quit)

    val reportFileChooser = new FileChooser
    val btnSave = new Button("Save")
    btnSave.reactions += onClick(chooseFile(reportFileChooser, saveToFile(txtArReport.text, _), btnSave))

    layout.row.grid(lblCsvFile).add(txtCsvFile, 5).add(btnChooseCsvFile)
    layout.row.grid(lblCsvSchemaFile).add(txtCsvSchemaFile, 5).add(btnChooseCsvSchemaFile)
    layout.row.right.add(cbFailOnFirstError).add(btnValidate)
    layout.row.center.fill.add(separator1)
    layout.row.center.fill.add(scrollPane)
    layout.row.center.fill.add(separator2)
    layout.row.right.add(btnSave).add(btnClose)

    panel
  }

  def top = new MainFrame {
    title = "CSV Validator"
    contents = content
  }
}
