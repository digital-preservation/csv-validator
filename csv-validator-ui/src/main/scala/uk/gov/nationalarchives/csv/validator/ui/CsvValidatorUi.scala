/**
 * Copyright (c) 2013, The National Archives <digitalpreservation@nationalarchives.gov.uk>
 * http://www.nationalarchives.gov.uk
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package uk.gov.nationalarchives.csv.validator.ui

import scala.swing._
import resource._
import javax.swing._
import net.java.dev.designgridlayout._
import java.io.{FileOutputStream, FileInputStream, File, PrintWriter}
import table.DefaultTableModel
import uk.gov.nationalarchives.csv.validator.cmd.CsvValidatorCmdApp
import swing.GridBagPanel.Anchor
import uk.gov.nationalarchives.csv.validator.ui.DesignGridImplicits._
import scala.swing.PopupMenuImplicits._
import ScalaSwingHelpers._
import java.awt.{Font, Cursor}
import java.util.Properties
import scalax.file.Path
import uk.gov.nationalarchives.csv.validator.ProgressCallback
import java.nio.charset.Charset
import uk.gov.nationalarchives.csv.validator.api.TextFile
import java.util.jar.{Manifest, Attributes}
import java.net.URL
import scala.swing.Dialog.Message
import scala.swing.event.ButtonClicked
import javax.swing
import scala.swing

/**
 * Simple GUI for the CSV Validator
 *
 * @author Adam Retter <adam.retter@googlemail.com>
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

  def top = new SJXFrame {
    title = "CSV Validator"
    contents = {
      val settings = new SettingsPanel

      //handle resizing the main window, when resizing the settings panel
      settings.settingsGroup.reactions += SJXTaskPane.onViewStateChanged {
        val newSize = if(settings.settingsGroup.collapsed) {
          new Dimension(this.size.getWidth.toInt, (this.size.getHeight - settings.size.getHeight).toInt)
        } else {
          new Dimension(this.size.getWidth.toInt, (this.size.getHeight + settings.size.getHeight).toInt)
        }
        this.size = newSize
        this.pack()
      }

      new ContentPanel(settings)
    }
  }

  private def getShortVersion(): String = {
    extractFromManifest {
      attributes =>
        attributes.getValue("Implementation-Version")
    }.getOrElse("UNKNOWN")
  }

  private def getLongVersion(): Seq[(String, String)] = {
    extractFromManifest {
      attributes =>
        Seq(
          ("Version", attributes.getValue("Implementation-Version")),
          ("Revision", attributes.getValue("Git-Commit")),
          ("Build Timestamp", attributes.getValue("Build-Timestamp"))
        )
    }.getOrElse(Seq(("Version", "UNKNOWN")))
  }

  private def extractFromManifest[T](extractor: Attributes => T): Option[T] = {
    val clazz = getClass()
    val className = clazz.getSimpleName + ".class"
    val classPath = clazz.getResource(className).toString()
    if (!classPath.startsWith("jar")) {
      None // Class not from JAR
    } else {
      val manifestPath = classPath.substring(0, classPath.lastIndexOf("!") + 1) + "/META-INF/MANIFEST.MF"
      managed(new URL(manifestPath).openStream()).map {
        is =>
          val manifest = new Manifest(is)
          extractor(manifest.getMainAttributes)
      }.opt
    }
  }

  private def displayWait(suspendUi: => Unit, action: (String => Unit) => Unit, output: String => Unit, resumeUi: => Unit) {
    import scala.concurrent.{future, Future}
    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.util.{Success, Failure}

    suspendUi
    val fAction: Future[Unit] = Future {
      action(output)
    }

    fAction.onComplete {
      case Success(_) =>
        Swing.onEDT { resumeUi }

      case Failure(t) =>
        t.printStackTrace()
        output(t.toString)
        Swing.onEDT {resumeUi }
    }
  }

  private def validate(csvFilePath: String, csvEncoding: Charset, csvSchemaFilePath: String, csvSchemaEncoding: Charset, failOnFirstError: Boolean, pathSubstitutions: List[(String, String)], enforceCaseSensitivePathChecks: Boolean, progress: Option[ProgressCallback])(output: String => Unit) {
    output("")
    output(CsvValidatorCmdApp.validate(
      TextFile(Path.fromString(csvFilePath), csvEncoding),
      TextFile(Path.fromString(csvSchemaFilePath), csvSchemaEncoding),
      failOnFirstError,
      pathSubstitutions,
      enforceCaseSensitivePathChecks,
      progress
    )._1)
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

  case class Settings(lastCsvPath: File, lastCsvSchemaPath: File, lastReportPath: File)

  lazy val settingsFile = new File(System.getProperty("user.home") + "/.csv-validator/csv-validator.properties")
  lazy val userDir = new File(System.getProperty("user.dir"))

  private def loadSettings: Option[Settings] = {
    if(settingsFile.exists()) {
      val props = new Properties
      managed(new FileInputStream(settingsFile)).map {
        is =>
          props.load(is)
          Settings(new File(props.getProperty("last.csv.path")), new File(props.getProperty("last.csv.uk.gov.nationalarchives.csv.validator.schema.path")), new File(props.getProperty("last.report.path")))
      }.opt
    } else {
      None
    }
  }

  private def saveSettings(settings: Settings) {
    if(!settingsFile.exists) {
      settingsFile.getParentFile.mkdirs
    }
    val props = new Properties
    props.setProperty("last.csv.path", settings.lastCsvPath.getAbsolutePath)
    props.setProperty("last.csv.uk.gov.nationalarchives.csv.validator.schema.path", settings.lastCsvSchemaPath.getAbsolutePath)
    props.setProperty("last.report.path", settings.lastReportPath.getAbsolutePath)
    managed(new FileOutputStream(settingsFile)).map {
      os =>
        props.store(os, "CSV Validator")
    }.opt.getOrElse(Unit)
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
    private val csvFileChooser = new FileChooser(loadSettings match {
      case Some(s) =>
        s.lastCsvPath
      case None =>
        userDir
    })
    private val btnChooseCsvFile = new Button("...")

    btnChooseCsvFile.reactions += onClick {
      chooseFile(csvFileChooser, txtCsvFile, btnChooseCsvFile)
      updateLastPath(csvFileChooser, {
        path =>
          loadSettings match {
            case Some(s) =>
              s.copy(lastCsvPath = path)
            case None =>
              Settings(path, path, path)
          }
      })
    }

    private val lblCsvSchemaFile = new Label("CSV Schema file:")
    private val txtCsvSchemaFile = new TextField(30)
    private val csvSchemaFileChooser = new FileChooser(loadSettings match {
      case Some(s) =>
        s.lastCsvSchemaPath
      case None =>
        userDir
    })
    private val btnChooseCsvSchemaFile = new Button("...")
    btnChooseCsvSchemaFile.reactions += onClick {
      chooseFile(csvSchemaFileChooser, txtCsvSchemaFile, btnChooseCsvSchemaFile)
      updateLastPath(csvSchemaFileChooser, {
        path =>
          loadSettings match {
            case Some(s) =>
              s.copy(lastCsvSchemaPath = path)
            case None =>
              Settings(path, path, path)
          }
      })
    }

    private val separator1 = new Separator

    private val scrollPane = new ScrollPane
    private val txtArReport = new TextArea(12,30)
    scrollPane.viewportView = txtArReport

    private val btnValidate = new Button("Validate")

    private val progressBar = new ProgressBar()
    progressBar.visible = false
    private val progress = new ProgressCallback {
      override def update(complete: this.type#Percentage) {
        Swing.onEDT {
          progressBar.value = complete.toInt
        }
      }
    }

    def outputToReport(data: String) {
      Swing.onEDT {
        txtArReport.text = data
      }
    }

    btnValidate.reactions += onClick(displayWait(
      suspendUi = {
        btnValidate.enabled = false
        this.peer.setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR))
        this.progressBar.value = 0
        this.progressBar.visible = true

      },
      action = CsvValidatorUi.this.validate(txtCsvFile.text, settingsPanel.csvEncoding, txtCsvSchemaFile.text, settingsPanel.csvSchemaEncoding, settingsPanel.failOnFirstError, settingsPanel.pathSubstitutions, settingsPanel.enforceCaseSensitivePathChecks, Some(progress)),
      output = outputToReport,
      resumeUi = {
        btnValidate.enabled = true
        this.peer.setCursor(Cursor.getDefaultCursor)
        //this.progressBar.visible = false
      }
    ))

    private val separator2 = new Separator

    private val btnClose = new Button("Close")
    btnClose.reactions += onClick(quit)

    private val reportFileChooser = new FileChooser(loadSettings match {
      case Some(s) =>
        s.lastReportPath
      case None =>
        userDir
    })
    private val btnSave = new Button("Save")
    btnSave.reactions += onClick {
      chooseFile(reportFileChooser, saveToFile(txtArReport.text, _), btnSave)
      updateLastPath(reportFileChooser, {
        path =>
          loadSettings match {
            case Some(s) =>
              s.copy(lastReportPath = path)
            case None =>
              Settings(path, path, path)
          }
      })
    }

    private val lblVersion = new Label(s"Version: ${getShortVersion()}")
    lblVersion.listenTo(lblVersion.mouse.clicks)
    lblVersion.font = lblVersion.font.deriveFont(9)
    lblVersion.reactions += onClick {
      Dialog.showMessage(this, getLongVersion().map(x => s"${x._1}: ${x._2}").mkString(System.getProperty("line.separator")), "Version Details")
    }

    layout.row.grid(lblCsvFile).add(txtCsvFile, 5).add(btnChooseCsvFile)
    layout.row.grid(lblCsvSchemaFile).add(txtCsvSchemaFile, 5).add(btnChooseCsvSchemaFile)

    layout.row.center.fill.add(settingsPanel)

    layout.row.center.fill.add(btnValidate)
    layout.row.center.fill.add(progressBar)

    layout.row.center.fill.add(separator1)
    layout.row.center.fill.add(scrollPane)
    layout.row.center.fill.add(separator2)

    layout.row.right.withOwnRowWidth.add(btnSave).add(btnClose)

    layout.row.grid(lblVersion)
  }

  def updateLastPath(fileChooser: FileChooser, sink: File => Settings) {
    Option(fileChooser.selectedFile) match {
      case Some(selection) =>
        saveSettings(sink(selection.getParentFile))
      case None =>
    }
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
  private class SettingsPanel extends SJXTaskPaneContainer {

    private lazy val CHARACTER_ENCODINGS =
      if(Charset.defaultCharset.name == "UTF-8") {
        Seq(Charset.defaultCharset)
      } else {
        Seq(Charset.forName("UTF-8"), Charset.defaultCharset)
      }

    val settingsGroup = new SJXTaskPane("Settings", true)
    private val lblCsvEncoding = new Label("CSV Encoding")
    private val cmbCsvEncoding = new ComboBox(CHARACTER_ENCODINGS)
    private val lblCsvSchemaEncoding = new Label("CSV Schema Encoding")
    private val cmbCsvSchemaEncoding = new ComboBox(CHARACTER_ENCODINGS)
    private val cbFailOnFirstError = new CheckBox("Fail on first error?")
    cbFailOnFirstError.tooltip = "Indicates whether to fail on the first error, or whether to collect all errors!"
    private val lblPathSubstitutions = new Label("Path Substitutions")
    private val cbEnforceCaseSensitivePathChecks = new CheckBox("Enforce case-sensitive file path checks?")
    cbEnforceCaseSensitivePathChecks.tooltip = "Performs additional checks to ensure that the case of file-paths in the CSV file match those of the filesystem"

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
      c.anchor = Anchor.West

      c.gridx = 0
      c.gridy = 0
      layout(lblCsvEncoding) = c

      c.gridx = 1
      c.gridy = 0
      layout(cmbCsvEncoding) = c

      c.gridx = 0
      c.gridy = 1
      layout(lblCsvSchemaEncoding) = c

      c.gridx = 1
      c.gridy = 1
      layout(cmbCsvSchemaEncoding) = c

      c.gridx = 0
      c.gridy = 2
      layout(cbFailOnFirstError) = c

      c.gridx = 0
      c.gridy = 3
      layout(cbEnforceCaseSensitivePathChecks) = c

      c.gridx = 0
      c.gridy = 4
      c.insets = new Insets(0,10,0,0)
      layout(lblPathSubstitutions) = c

      c.gridx = 0
      c.gridy = 5
      c.gridwidth = 2
      layout(spTblPathSubstitutions) = c

      c.gridx = 1
      c.gridy = 6
      c.anchor = Anchor.LastLineEnd
      layout(btnAddPathSubstitution) = c
    }
    settingsGroup.add(settingsGroupLayout)

    add(settingsGroup)

    def csvEncoding: Charset = cmbCsvEncoding.selection.item
    def csvSchemaEncoding: Charset = cmbCsvSchemaEncoding.selection.item
    def failOnFirstError: Boolean = cbFailOnFirstError.selected
    def pathSubstitutions: List[(String, String)] = tblPathSubstitutions.pathSubstitutions
    def enforceCaseSensitivePathChecks: Boolean = cbEnforceCaseSensitivePathChecks.selected
  }
}
