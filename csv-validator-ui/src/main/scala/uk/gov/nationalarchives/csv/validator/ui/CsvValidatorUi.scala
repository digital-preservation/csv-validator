/*
 * Copyright (c) 2013, The National Archives <digitalpreservation@nationalarchives.gov.uk>
 * https://www.nationalarchives.gov.uk
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package uk.gov.nationalarchives.csv.validator.ui

import cats.data.Validated.Invalid
import cats.data.ValidatedNel
import net.java.dev.designgridlayout._
import uk.gov.nationalarchives.csv.validator.api.TextFile
import uk.gov.nationalarchives.csv.validator.cmd.{CsvValidatorCmdApp, SystemExitCodes}
import uk.gov.nationalarchives.csv.validator.ui.DesignGridImplicits._
import uk.gov.nationalarchives.csv.validator.ui.ScalaSwingHelpers._
import uk.gov.nationalarchives.csv.validator.{EOL, FailMessage, ProgressCallback}

import java.awt.Cursor
import java.awt.datatransfer.DataFlavor
import java.io.{File, IOException}
import java.net.URL
import java.nio.charset.Charset
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, Paths, StandardOpenOption}
import java.util
import java.util.Properties
import java.util.jar.{Attributes, Manifest}
import javax.swing._
import javax.swing.filechooser.FileNameExtensionFilter
import javax.swing.table.DefaultTableModel
import scala.jdk.CollectionConverters.CollectionHasAsScala
import scala.language.reflectiveCalls
import scala.swing.GridBagPanel.Anchor
import scala.swing.PopupMenuImplicits._
import scala.swing._
import scala.util.{Failure, Success, Try, Using}

/**
 * Simple GUI for the CSV Validator
 *
 * @author Adam Retter <adam.retter@googlemail.com>
 */
object CsvValidatorUi extends SimpleSwingApplication {
  override def startup(args: Array[String]) : Unit = {
    try {
      UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName)
    } catch {
      case ue : UnsupportedLookAndFeelException =>
        JOptionPane.showMessageDialog(this.top.peer, "Unable to set native look and feel: " + ue.getMessage, "Look and Feel Warning", JOptionPane.WARNING_MESSAGE)
    }
    super.startup(args)
  }

  def top: SJXFrame = new SJXFrame {

    title = "CSV Validator"
    peer.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
    contents = {
      val settings = new SettingsPanel(this)

      //handle resizing the main window, when resizing the settings panel
      settings.settingsGroup.reactions += SJXTaskPane.onViewStateChanged {
        val newSize = if(settings.settingsGroup.collapsed) {
          new Dimension(this.size.getWidth.toInt, (this.size.getHeight - settings.size.getHeight).toInt)
        } else {
          new Dimension(this.size.getWidth.toInt, (this.size.getHeight + settings.size.getHeight).toInt)
        }
        this.preferredSize = newSize
        this.pack()
      }
      new ContentPanel(settings)
    }
  }

  private def getShortVersion: String = {
    extractFromManifest {
      attributes =>
        attributes.getValue("Implementation-Version")
    }.getOrElse("UNKNOWN")
  }

  private def getLongVersion: Seq[(String, String)] = {
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
      Using(new URL(manifestPath).openStream()) {
        is =>
          val manifest = new Manifest(is)
          extractor(manifest.getMainAttributes)
      }.map(Some(_)).getOrElse(None)
    }
  }

  private def displayWait(suspendUi: => Unit, action: (String => Unit) => Unit, output: String => Unit, resumeUi: => Unit) : Unit = {
    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent.Future
    import scala.util.{Failure, Success}

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

  private def validate(csvFilePath: String, csvEncoding: Charset, csvSchemaFilePath: String, csvSchemaEncoding: Charset, failOnFirstError: Boolean, pathSubstitutions: List[(String, String)], enforceCaseSensitivePathChecks: Boolean, progress: Option[ProgressCallback], validateEncoding: Boolean)(output: String => Unit) : Unit = {

    def toConsole(msg: String): Unit = Swing.onEDT {
      output(msg)
    }

    var badLines = 0
    var truncated = false

    def rowCallback(row: ValidatedNel[FailMessage, Any]): Unit = row match {

      case Invalid(failures) =>
        if (badLines > 2000) {
          if (!truncated) {
            toConsole("Too many errors/warnings, truncating")
            truncated = true
          }
        } else {
          toConsole(CsvValidatorCmdApp.prettyPrint(failures))
        }

        badLines += failures.size

      case _ =>
    }

    val cliResult = CsvValidatorCmdApp.validate(
      TextFile(Paths.get(csvFilePath), csvEncoding, validateEncoding),
      TextFile(Paths.get(csvSchemaFilePath), csvSchemaEncoding),
      failOnFirstError,
      pathSubstitutions,
      enforceCaseSensitivePathChecks,
      trace = false,
      progress,
      rowCallback
    )

    cliResult._2 match {
      case SystemExitCodes.ValidCsv => toConsole("PASS")
      case _ => toConsole(cliResult._1)
    }
  }

  /**
   * Saves a String to a File
   *
   * @param s String to save to the file
   * @param f File to which the associated string is saved
   */
  private def saveToFile(s: String, f: Path) : Option[IOException] = {
    val data : Array[Byte] =  s.getBytes(UTF_8)
    try {
      Files.write(f, data, StandardOpenOption.WRITE, StandardOpenOption.CREATE_NEW)
      None
    } catch {
      case ioe: IOException => Some(ioe)
    }
  }

  case class Settings(lastCsvPath: Path, lastCsvSchemaPath: Path, lastReportPath: Path)

  lazy val settingsFile : Path = Paths.get(System.getProperty("user.home")).resolve(".csv-validator").resolve("csv-validator.properties")
  lazy val userDir: Path = Paths.get(System.getProperty("user.dir"))

  private def loadSettings: Option[Settings] = {
    if(Files.exists(settingsFile)) {
      val props = new Properties
      Using(Files.newInputStream(settingsFile)) {
        is =>
          props.load(is)
          Settings(Paths.get(props.getProperty("last.csv.path")), Paths.get(props.getProperty("last.csv.uk.gov.nationalarchives.csv.validator.schema.path")), Paths.get(props.getProperty("last.report.path")))
      }.map(Some(_)).getOrElse(None)
    } else {
      None
    }
  }

  private def saveSettings(settings: Settings) : Unit = {
    if(!Files.exists(settingsFile)) {
      Files.createDirectories(settingsFile.getParent)
    }
    val props = new Properties
    props.setProperty("last.csv.path", settings.lastCsvPath.normalize.toAbsolutePath.toString)
    props.setProperty("last.csv.uk.gov.nationalarchives.csv.validator.schema.path", settings.lastCsvSchemaPath.normalize.toAbsolutePath.toString)
    props.setProperty("last.report.path", settings.lastReportPath.normalize.toAbsolutePath.toString)
    Using(Files.newOutputStream(settingsFile)) {
      os =>
        props.store(os, "CSV Validator")
    }.map(Some(_)).getOrElse(None)
  }

  private val txtArReport = new TextArea(12,30)

  /**
   * The main UI of the application
   *
   * @param settingsPanel The settings panel for the UI
   */
  private class ContentPanel(settingsPanel: SettingsPanel) extends Panel {

    private val layout = new DesignGridLayout(peer)

    private val lblCsvFile = new Label("CSV file:")
    private val txtCsvFile = new JTextField(30)

    final class FileDropHandler(fileExt: String, label: Label) extends TransferHandler {
      override def canImport(support: TransferHandler.TransferSupport): Boolean = support.getDataFlavors.exists(_.isFlavorJavaFileListType)

      @SuppressWarnings(Array("unchecked"))
      override def importData(support: TransferHandler.TransferSupport): Boolean = {
        if (!this.canImport(support)) false
        else {
          val potentialFiles = Try {
            support.getTransferable.getTransferData(DataFlavor.javaFileListFlavor).asInstanceOf[util.List[File]].asScala.toList
          }
          potentialFiles match {
            case Failure(ex) => false
            case Success(files) =>
              lazy val pathOfFirstFile = files.head.getAbsolutePath
              if(files.length != 1) {
                outputToReport(s"Error: Please drag only 1 file into the '${label.text}' text box.")
                false
              }
              else if(!pathOfFirstFile.endsWith(fileExt)) {
                outputToReport(s"Error: Please drag only '$fileExt' files into the '${label.text}' text box.")
                false
              }
              else {
                (if(fileExt == ".csv") txtCsvFile else txtCsvSchemaFile).setText(pathOfFirstFile)
                true
              }
          }
        }
      }
    }
    txtCsvFile.setTransferHandler(new FileDropHandler(".csv", lblCsvFile))
    private val csvFileChooser = new FileChooser(loadSettings match {
      case Some(s) =>
        s.lastCsvPath.toFile
      case None =>
        userDir.toFile
    })
    csvFileChooser.fileFilter = new FileNameExtensionFilter("CSV file (*.csv)", "csv")
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
    private val txtCsvSchemaFile = new JTextField(30)
    txtCsvSchemaFile.setTransferHandler(new FileDropHandler(".csvs", lblCsvSchemaFile))
    private val csvSchemaFileChooser = new FileChooser(loadSettings match {
      case Some(s) =>
        s.lastCsvSchemaPath.toFile
      case None =>
        userDir.toFile
    })
    csvSchemaFileChooser.fileFilter = new FileNameExtensionFilter("CSV Schema file (*.csvs)", "csvs" +
      "")

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
    scrollPane.viewportView = txtArReport

    private val btnValidate = new Button("Validate")

    private val progressBar = new ProgressBar()
    progressBar.visible = false
    progressBar.labelPainted = true
    progressBar.label = ""
    private val progress = new ProgressCallback {
      override def update(complete: this.type#Percentage) : Unit = {
        Swing.onEDT {
          progressBar.label = null
          progressBar.value = complete.toInt
        }
      }

      override def update(total: Int, processed: Int) : Unit = {
        Swing.onEDT {
          progressBar.max = total
          progressBar.value = processed
          progressBar.label = s"Line ${processed} of ${total}"
        }
      }
    }

    def outputToReport(data: String) : Unit =
      Swing.onEDT {
        txtArReport.append(data + EOL)
      }


    btnValidate.reactions += onClick(displayWait(
      suspendUi = {
        btnValidate.enabled = false
        this.peer.setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR))
        this.progressBar.value = 0
        this.progressBar.visible = true

      },
      action = {
        txtArReport.text = ""
        CsvValidatorUi.this.validate(
          txtCsvFile.getText,
          settingsPanel.csvEncoding,
          txtCsvSchemaFile.getText,
          settingsPanel.csvSchemaEncoding,
          settingsPanel.failOnFirstError,
          settingsPanel.pathSubstitutions,
          settingsPanel.enforceCaseSensitivePathChecks,
          Some(progress),
          settingsPanel.validateUtf8
        )
      },
      output = outputToReport,
      resumeUi = {
        btnValidate.enabled = true
        this.peer.setCursor(Cursor.getDefaultCursor)
        //this.progressBar.visible = false
      }
    ))

    private val separator2 = new Separator

    private val btnClose = new Button("Close")
    btnClose.reactions += onClick(quit())

    private val reportFileChooser = new FileChooser(loadSettings match {
      case Some(s) =>
        s.lastReportPath.toFile
      case None =>
        userDir.toFile
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

    private val lblVersion = new Label(s"Version: ${getShortVersion}")
    lblVersion.listenTo(lblVersion.mouse.clicks)
    lblVersion.font = lblVersion.font.deriveFont(9)
    lblVersion.reactions += onClick {
      Dialog.showMessage(this, getLongVersion.map(x => s"${x._1}: ${x._2}").mkString(System.getProperty("line.separator")), "Version Details")
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

  def updateLastPath(fileChooser: FileChooser, sink: Path => Settings) : Unit = {
    Option(fileChooser.selectedFile) match {
      case Some(selection) =>
        saveSettings(sink(selection.toPath.getParent))
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
  private class SettingsPanel(parentFrame: SJXFrame) extends SJXTaskPaneContainer {

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
    private val cbValidateUtf8 = new CheckBox("Validate csv for valid UTF-8 characters")
    cbValidateUtf8.selected = true
    private val lblPathSubstitutions = new Label("Path Substitutions")
    private val cbEnforceCaseSensitivePathChecks = new CheckBox("Enforce case-sensitive file path checks?")
    cbEnforceCaseSensitivePathChecks.tooltip = "Performs additional checks to ensure that the case of file-paths in the CSV file match those of the filesystem"

    private val tblPathSubstitutions = new Table(0, 2) {
      preferredViewportSize = new Dimension(500, 70)
      model = new DefaultTableModel(Array[Object]("From", "To"), 0)

      def addRow(rowData: Array[String]) : Unit = {
        model.asInstanceOf[DefaultTableModel].addRow(rowData.asInstanceOf[Array[AnyRef]])
      }

      def removeSelectedRow() : Unit = {
        model.asInstanceOf[DefaultTableModel].removeRow(peer.getSelectedRow)
      }

      def pathSubstitutions : List[(String, String)] = {
        for(rowIdx <- (0 to model.getRowCount - 1)) yield (model.getValueAt(rowIdx, 0).asInstanceOf[String], model.getValueAt(rowIdx, 1).asInstanceOf[String])
      }.toList
    }

    private val popupMenu = new PopupMenu
    private val miRemove = new MenuItem("Remove Path Substitution")
    miRemove.reactions += onClick(tblPathSubstitutions.removeSelectedRow())
    popupMenu.contents += miRemove
    tblPathSubstitutions.popupMenu(popupMenu)

    private val spTblPathSubstitutions = new ScrollPane(tblPathSubstitutions)
    private val btnAddPathSubstitution = new Button("Add Path Substitution...")
    btnAddPathSubstitution.reactions += onClick(addToTableDialog(parentFrame, "Add Path Substitution...", tblPathSubstitutions, tblPathSubstitutions.addRow))

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
      layout(cbValidateUtf8) = c

      c.gridx = 0
      c.gridy = 5
      c.insets = new Insets(0,10,0,0)
      layout(lblPathSubstitutions) = c

      c.gridx = 0
      c.gridy = 6
      c.gridwidth = 2
      layout(spTblPathSubstitutions) = c

      c.gridx = 1
      c.gridy = 7
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
    def validateUtf8 : Boolean = cbValidateUtf8.selected
  }
}
