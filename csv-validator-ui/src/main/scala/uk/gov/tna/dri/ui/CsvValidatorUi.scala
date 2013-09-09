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
import java.io.{File, PrintWriter}
import table.DefaultTableModel
import uk.gov.tna.dri.validator.MetaDataValidatorCommandLineApp
import swing.GridBagPanel.Anchor
import uk.gov.tna.dri.ui.DesignGridImplicits._
import scala.swing.PopupMenuImplicits._
import ScalaSwingHelpers._
import java.awt.Cursor

/**
 * Simple GUI for the CSV Validator
 */
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

  def top = new MainFrame {
    title = "CSV Validator"
    contents = new ContentPanel(new SettingsPanel)
  }

  private def displayWait(suspendUi: => Unit, action: (String => Unit) => Unit, output: String => Unit, resumeUi: => Unit) {
    try {
      suspendUi
      action(output)
    } catch {
      case e: Throwable =>
        output(e.toString)
        e.printStackTrace()
    } finally {
      resumeUi
    }
  }

  private def validate(csvFilePath: String, csvSchemaFilePath: String, failOnFirstError: Boolean, pathSubstitutions: List[(String, String)])(output: String => Unit) {
    output("")
    output(MetaDataValidatorCommandLineApp.processMetaData(csvFilePath, csvSchemaFilePath, failOnFirstError, pathSubstitutions)._1)
  }

  /**
   * Saves a String to a File
   *
   * @param s
   * @param f
   */
  private def saveToFile(s: String, f: File) {
    val printWriter = new PrintWriter(f)
    try {
      printWriter.write(s)
    } finally {
      printWriter.close
    }
  }

  /**
   * The main UI of the application
   *
   * @param settingsPanel The settings panel for the UI
   */
  private class ContentPanel(settingsPanel: SettingsPanel) extends Panel {

    private val layout = new DesignGridLayout(peer)

    private val lblCsvFile = new Label("CSV file:")
    private val txtCsvFile = new TextField(30)
    private val csvFileChooser = new FileChooser
    private val btnChooseCsvFile = new Button("...")
    btnChooseCsvFile.reactions += onClick(chooseFile(csvFileChooser, txtCsvFile, btnChooseCsvFile))

    private val lblCsvSchemaFile = new Label("CSV Schema file:")
    private val txtCsvSchemaFile = new TextField(30)
    private val csvSchemaFileChooser = new FileChooser
    private val btnChooseCsvSchemaFile = new Button("...")
    btnChooseCsvSchemaFile.reactions += onClick(chooseFile(csvSchemaFileChooser, txtCsvSchemaFile, btnChooseCsvSchemaFile))

    private val separator1 = new Separator

    private val scrollPane = new ScrollPane
    private val txtArReport = new TextArea(12,30)
    scrollPane.viewportView = txtArReport

    private val btnValidate = new Button("Validate")

    val outputToReport: String => Unit = {
      txtArReport.text = _
    }

    btnValidate.reactions += onClick(displayWait(
      suspendUi = {
        btnValidate.enabled = false
        this.peer.setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR))
      },
      action = validate(txtCsvFile.text, txtCsvSchemaFile.text, settingsPanel.failOnFirstError, settingsPanel.pathSubstitutions),
      output = outputToReport,
      resumeUi = {
        btnValidate.enabled = true
        this.peer.setCursor(Cursor.getDefaultCursor)
      }
    ))

    private val separator2 = new Separator

    private val btnClose = new Button("Close")
    btnClose.reactions += onClick(quit)

    private val reportFileChooser = new FileChooser
    private val btnSave = new Button("Save")
    btnSave.reactions += onClick(chooseFile(reportFileChooser, saveToFile(txtArReport.text, _), btnSave))

    layout.row.grid(lblCsvFile).add(txtCsvFile, 5).add(btnChooseCsvFile)
    layout.row.grid(lblCsvSchemaFile).add(txtCsvSchemaFile, 5).add(btnChooseCsvSchemaFile)

    layout.row.center.fill.add(settingsPanel)

    layout.row.center.fill.add(btnValidate)

    layout.row.center.fill.add(separator1)
    layout.row.center.fill.add(scrollPane)
    layout.row.center.fill.add(separator2)

    layout.row.right.withOwnRowWidth.add(btnSave).add(btnClose)
  }

  /**
   * A UI aspect for the Settings available
   * when calling CSV Validator
   *
   * This was only separated from ContentPanel
   * because it is self-contained and allowed
   * us to break up the code easily, hopefully
   * making it more understandable.
   */
  private class SettingsPanel extends TaskPaneContainer {

    private val settingsGroup = new TaskPane("Settings", true)
    private val cbFailOnFirstError = new CheckBox("Fail on first error?")
    private val lblPathSubstitutions = new Label("Path Substitutions")

    private val tblPathSubstitutions = new Table(0, 2) {
      preferredViewportSize = new Dimension(500, 70)
      model = new DefaultTableModel(Array[Object]("From", "To"), 0)

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

    private val popupMenu = new PopupMenu
    private val miRemove = new MenuItem("Remove Path Substitution")
    miRemove.reactions += onClick(tblPathSubstitutions.removeSelectedRow)
    popupMenu.contents += miRemove
    tblPathSubstitutions.popupMenu(popupMenu)

    private val spTblPathSubstitutions = new ScrollPane(tblPathSubstitutions)
    private val btnAddPathSubstitution = new Button("Add Path Substitution...")
    btnAddPathSubstitution.reactions += onClick(addToTableDialog(top, "Add Path Substitution...", tblPathSubstitutions, tblPathSubstitutions.addRow))

    private val settingsGroupLayout = new GridBagPanel {
      private val c = new Constraints
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

    add(settingsGroup)

    def failOnFirstError: Boolean = cbFailOnFirstError.selected
    def pathSubstitutions: List[(String, String)] = tblPathSubstitutions.pathSubstitutions
  }
}
