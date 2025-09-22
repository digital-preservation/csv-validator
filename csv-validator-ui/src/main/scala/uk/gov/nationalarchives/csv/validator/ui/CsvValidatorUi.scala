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
import java.text.SimpleDateFormat
import java.util
import java.util.{Date, Properties}
import java.util.jar.{Attributes, Manifest}
import javax.swing.SpringLayout.Constraints
import javax.swing._
import javax.swing.filechooser.FileNameExtensionFilter
import javax.swing.table.DefaultTableModel
import scala.jdk.CollectionConverters.CollectionHasAsScala
import scala.language.reflectiveCalls
import scala.swing.FileChooser.SelectionMode
import scala.swing.GridBagPanel.Anchor
import scala.swing.PopupMenuImplicits._
import scala.swing._
import scala.swing.event.ButtonClicked
import scala.util.{Failure, Success, Try, Using}

/**
 * Simple GUI for the CSV Validator
 *
 * @author Adam Retter <adam.retter@googlemail.com>
 */
object CsvValidatorUi extends SimpleSwingApplication {
  var prefSizeHeightChanged = true

  override def startup(args: Array[String]) : Unit = {
    try {
      UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName)
    } catch {
      case ue : UnsupportedLookAndFeelException =>
        JOptionPane.showMessageDialog(this.top.peer, "Unable to set native look and feel: " + ue.getMessage, "Look and Feel Warning", JOptionPane.WARNING_MESSAGE)
    }
    super.startup(args)
  }

  private lazy val txtCsvFile = new JTextField(30)
  private lazy val txtCsvSchemaFile = new JTextField(30)

  private def changeHeightOfOutputPane(heightDiff: Int): Unit = {
    // need to normalise the height change by converting it to "rows"
    val heightChangeAsRows = Math.ceil(heightDiff / 30f).toInt // 30 is just an arbitrary number that seemed to work fine
    val newHeight = if(txtArReport.rows + heightChangeAsRows <= 1) 1
    else if(txtArReport.rows + heightChangeAsRows > 40) 40
    else txtArReport.rows + heightChangeAsRows

    // sometimes the size and preferredSize height stay the same even when the window gets larger so just make box size large in this case
    val finalHeight = if(heightDiff == 0 && !prefSizeHeightChanged) 40 else newHeight
    txtArReport.rows = finalHeight
  }

  def top: SJXFrame = new SJXFrame {
    this.listenTo(this) // add a listener to the window to listen for all changes to the window
    private var previousHeight = this.preferredSize.height
    this.reactions += onResize {
      val heightDiff = this.size.height - this.preferredSize.height

      changeHeightOfOutputPane(heightDiff)
      prefSizeHeightChanged = previousHeight != this.preferredSize.height
      previousHeight = this.preferredSize.height
    }

    title = "CSV Validator"
    peer.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
    contents = {
      val settings = new SettingsPanel(this)

      //handle resizing the main window, when resizing the settings panel
      settings.settingsGroup.reactions += SJXTaskPane.onViewStateChanged {
        val newSize = if(settings.settingsGroup.collapsed) {
          txtArReport.rows = 40
          this.size
        } else {
          txtArReport.rows = 9
          val settingsHeight = settings.size.getHeight
          new Dimension(this.size.getWidth.toInt, (this.preferredSize.getHeight + settingsHeight).toInt)
        }
        this.preferredSize = newSize
        this.pack()
      }
      new ContentPanel(settings, this)
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
    val clazz = getClass
    val className = clazz.getSimpleName + ".class"
    val classPath = clazz.getResource(className).toString
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

  private def convertTextboxValueToInt(value: String, textBoxDescription: String, handleErrorMsg: String => Unit, valueIfError: Int = 0) =
    Try(value.toInt) match {
      case Success(number) if number > 0 => number
      case _ =>
        handleErrorMsg(s"Error:   Maximum number of $textBoxDescription should be more than 0")
        valueIfError
    }

  private def validate(csvFilePath: String, csvEncoding: Charset, csvSchemaFilePath: String, csvSchemaEncoding: Charset,
                       maxCharsPerCell: Int, failOnFirstError: Boolean, pathSubstitutions: List[(String, String)],
                       enforceCaseSensitivePathChecks: Boolean, progress: Option[ProgressCallback], validateEncoding: Boolean,
                       maxNumOfLines: Int, skipFileChecks: Boolean, outputTextSuffix: String)(output: String => Unit) : Unit = {

    def toConsole(msg: String): Unit = Swing.onEDT {
      output(msg)
    }

    var badLines = 0
    var truncated = false

    def logRowCallback(maxBadLines: Int)(row: ValidatedNel[FailMessage, Any]): Unit = row match {
      case Invalid(failures) =>
        if (badLines >= maxBadLines) {
          if (!truncated) {
            toConsole(
              s"...\n\nNote: Number of errors to display has reached the set limit of $maxBadLines; " +
                "increase this limit and re-run in order to display more errors."
            )
            truncated = true
          }
        } else toConsole(CsvValidatorCmdApp.prettyPrint(failures))

        badLines += failures.size

      case _ =>
    }

      val (status, cliExitCode) = CsvValidatorCmdApp.validate(
        TextFile(Paths.get(csvFilePath), csvEncoding, validateEncoding),
        TextFile(Paths.get(csvSchemaFilePath), csvSchemaEncoding),
        failOnFirstError,
        pathSubstitutions,
        enforceCaseSensitivePathChecks,
        trace = false,
        maxCharsPerCell,
        progress,
        skipFileChecks,
        logRowCallback(maxNumOfLines)
      )

      cliExitCode match {
        case SystemExitCodes.ValidCsv => toConsole(s"PASS$outputTextSuffix")
        case _ => toConsole(status)
      }
  }

  /**
   * Saves a String to a File
   *
   * @param s String to save to the file
   * @param f File to which the associated string is saved
   */
  private def saveToFile(s: String, f: Path) : Try[String] = {
    val data : Array[Byte] = s.getBytes(UTF_8)
    Try(Files.write(f, data, StandardOpenOption.WRITE, StandardOpenOption.CREATE_NEW)).map(_ => "s has been written to file")
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

  private val txtArReport = new TextArea()
  txtArReport.lineWrap = true

  /**
   * The main UI of the application
   *
   * @param settingsPanel The settings panel for the UI
   */
  private class ContentPanel(settingsPanel: SettingsPanel, parentFrame: SJXFrame) extends Panel {
    private val lblCsvFile = new Label("CSV file:")
    val fileHandler = new FileDropHandler
    peer.setTransferHandler(fileHandler)
    private val layout = new DesignGridLayout(peer)

    private def showErrorDialog(message: String) = {
      JOptionPane.showMessageDialog(parentFrame.peer, message, "Error", JOptionPane.ERROR_MESSAGE)
      false
    }

    txtCsvFile.setTransferHandler(fileHandler)
    final class FileDropHandler extends TransferHandler {
      override def canImport(support: TransferHandler.TransferSupport): Boolean = support.getDataFlavors.exists(_.isFlavorJavaFileListType)

      @SuppressWarnings(Array("unchecked"))
      override def importData(support: TransferHandler.TransferSupport): Boolean =
        if (!this.canImport(support)) false
        else {
          val potentialFiles = Try {
            support.getTransferable.getTransferData(DataFlavor.javaFileListFlavor).asInstanceOf[util.List[File]].asScala.toList
          }
          potentialFiles match {
            case Failure(ex) => false
            case Success(files) =>
              val numOfFilesDropped = files.length
              lazy val filePaths = files.map(_.getAbsolutePath)
              lazy val (file1Ext, file2Ext) = {
                val fileExtensions = filePaths.map(_.split('.').last.toLowerCase)
                (fileExtensions.head, fileExtensions.last)
              }

              if(numOfFilesDropped > 2) showErrorDialog("Drop a maximum of 2 files.")
              else if (numOfFilesDropped == 2 && Set(file1Ext, file2Ext) != Set("csv", "csvs"))
                showErrorDialog("Drop a single '.csv' file and its corresponding '.csvs' file.")
              else filePaths.map { filePath =>
                val lowercasedFilePath = filePath.toLowerCase
                if(lowercasedFilePath.endsWith(".csv")) {
                  txtCsvFile.setText(filePath)
                  true
                }
                else if(lowercasedFilePath.endsWith(".csvs")) {
                  txtCsvSchemaFile.setText(filePath)
                  true
                }
                else showErrorDialog("Drop only '.csv' and '.csvs' files.")
              }.head
          }
        }
    }

    private def lastCsvPath: File =
      loadSettings.flatMap { settings =>
        List(settings.lastCsvPath.toFile, settings.lastCsvSchemaPath.toFile, settings.lastReportPath.toFile).find(_ != userDir.toFile)
      }.getOrElse(userDir.toFile)

    private val csvFileChooser = new FileChooser(lastCsvPath)
    csvFileChooser.multiSelectionEnabled = false
    csvFileChooser.title = "Select a .csv file"
    csvFileChooser.fileFilter = new FileNameExtensionFilter("CSV file (*.csv)", "csv")
    private val btnChooseCsvFile = new Button("Choose...")

    btnChooseCsvFile.reactions += onClick {
      csvFileChooser.selectedFile = lastCsvPath
      chooseFileOrDir(csvFileChooser, txtCsvFile, btnChooseCsvFile)
      updateLastPath(csvFileChooser, path => loadSettings match {
        case Some(s) => s.copy(lastCsvPath = path)
        case None => Settings(path, path, path)
      })
    }

    private val lblCsvSchemaFile = new Label("CSV Schema file:")

    txtCsvSchemaFile.setTransferHandler(fileHandler)
    private def lastCsvSchemaPath: File =
      loadSettings.flatMap { settings =>
        List(settings.lastCsvSchemaPath.toFile, settings.lastCsvPath.toFile, settings.lastReportPath.toFile).find(_ != userDir.toFile)
      }.getOrElse(userDir.toFile)
    private val csvSchemaFileChooser = new FileChooser(lastCsvSchemaPath)
    csvSchemaFileChooser.multiSelectionEnabled = false
    csvSchemaFileChooser.title = "Select a .csvs file"
    csvSchemaFileChooser.fileFilter = new FileNameExtensionFilter("CSV Schema file (*.csvs)", "csvs" +
      "")

    private val btnChooseCsvSchemaFile = new Button("Choose...")
    btnChooseCsvSchemaFile.reactions += onClick {
      csvSchemaFileChooser.selectedFile = lastCsvSchemaPath
      chooseFileOrDir(csvSchemaFileChooser, txtCsvSchemaFile, btnChooseCsvSchemaFile)
      updateLastPath(csvSchemaFileChooser, path => loadSettings match {
        case Some(s) => s.copy(lastCsvSchemaPath = path)
        case None => Settings(path, path, path)
      })
    }

    private val separator1 = new Separator

    private val scrollPane = new ScrollPane
    txtArReport.peer.setTransferHandler(fileHandler)
    scrollPane.preferredSize = new Dimension(300, 70)  // for some reason, line wap only works if this is here
    scrollPane.viewportView = txtArReport

    private val btnValidate = new Button("Validate")
      btnValidate.tooltip = "Full metadata and file validation"
    private val btnValidateMetadataOnly = new Button("Validate Metadata Only")
      btnValidateMetadataOnly.tooltip = """Metadata validation, but "fileExists", "checksum" and "integrityCheck" checks not performed"""

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
          progressBar.label = s"Row $processed of $total"
        }
      }
    }

    def outputToReport(clearOutput: Boolean=false)(data: String) : Unit =
      Swing.onEDT {
        if(clearOutput) txtArReport.text = "" else ()
        txtArReport.append(data + EOL)
      }


    btnValidate.reactions += onClick(validateOnClick(false))
    btnValidateMetadataOnly.reactions += onClick(validateOnClick(true))

    private def validateOnClick(skipFileChecks: Boolean) = {
      val csvFilePath = txtCsvFile.getText
      val csvSchemaFilePath = txtCsvSchemaFile.getText
      val fileBoxesAreFilled = csvFilePath.nonEmpty && csvSchemaFilePath.nonEmpty

      lazy val maxCharsPerCell = convertTextboxValueToInt(settingsPanel.maxCharsPerCell, "characters per column", outputToReport(clearOutput = true))
      lazy val maxNumOfLines = convertTextboxValueToInt(settingsPanel.numOfLinesToDisplay, "errors to display", outputToReport(clearOutput = true))

      if(fileBoxesAreFilled && maxCharsPerCell > 0 && maxNumOfLines > 0){
        val suffix = if(skipFileChecks) " (Metadata Only)" else ""
        displayWait(
          suspendUi = {
            btnValidate.enabled = false
            btnValidateMetadataOnly.enabled = false
            this.peer.setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR))
            this.progressBar.value = 0
            this.progressBar.visible = true

          },
          action = {
            txtArReport.text = ""
            CsvValidatorUi.this.validate(
              csvFilePath,
              settingsPanel.csvEncoding,
              csvSchemaFilePath,
              settingsPanel.csvSchemaEncoding,
              maxCharsPerCell,
              settingsPanel.failOnFirstError,
              settingsPanel.pathSubstitutions,
              settingsPanel.enforceCaseSensitivePathChecks,
              Some(progress),
              settingsPanel.validateUtf8,
              maxNumOfLines,
              skipFileChecks,
              suffix
            )
          },
          output = outputToReport(),
          resumeUi = {
            btnValidate.enabled = true
            btnValidateMetadataOnly.enabled = true
            this.peer.setCursor(Cursor.getDefaultCursor)
          }
        )
      } else doNothingOnClick()
    }

    private val separator2 = new Separator

    private val btnClose = new Button("Close")
    btnClose.reactions += onClick(quit())

    private def lastReportPath: File =
      loadSettings.flatMap { settings =>
        List(settings.lastReportPath.toFile, settings.lastCsvSchemaPath.toFile, settings.lastCsvPath.toFile).find(_ != userDir.toFile)
      }.getOrElse(userDir.toFile)

    private val reportFileChooser = new FileChooser(lastReportPath)
    reportFileChooser.multiSelectionEnabled = false

    val saveLabel = "Save Results..."
    reportFileChooser.title = saveLabel
    private val btnSave = new Button(saveLabel)

    val dateFormat = new SimpleDateFormat("dd-mm-yy_HH-mm-ss")
    btnSave.reactions += onClick {
      reportFileChooser.selectedFile = Paths.get(lastReportPath.toString, s"csv_validator_report_${dateFormat.format(new Date())}.txt").toFile
      saveFile(reportFileChooser, saveToFile(txtArReport.text, _), btnSave, btnSave.text)
      updateLastPath(reportFileChooser, path => loadSettings match {
        case Some(s) => s.copy(lastReportPath = path)
        case None => Settings(path, path, path)
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
    layout.row.center.fill.add(btnValidateMetadataOnly)
    layout.row.center.fill.add(progressBar)

    layout.row.center.fill.add(separator1)
    layout.row.center.fill.add(scrollPane)
    layout.row.center.fill.add(separator2)

    layout.row.right.withOwnRowWidth.add(btnSave).add(btnClose)

    layout.row.grid(lblVersion)
  }

  def updateLastPath(fileChooser: FileChooser, sink: Path => Settings) : Unit = {
    Option(fileChooser.selectedFile) match {
      case Some(selection) => saveSettings(sink(selection.toPath))
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
    private val lblCsvEncoding = new Label("CSV Encoding:")
    private val cmbCsvEncoding = new ComboBox(CHARACTER_ENCODINGS)
    private val lblCsvSchemaEncoding = new Label("CSV Schema Encoding:")
    private val cmbCsvSchemaEncoding = new ComboBox(CHARACTER_ENCODINGS)
    private val cbFailOnFirstError = new CheckBox("Fail on first error")
    cbFailOnFirstError.tooltip = "Indicates whether to fail on the first error, or whether to collect all errors!"
    private val cbValidateUtf8 = new CheckBox("Validate CSV for valid UTF-8 characters")
    cbValidateUtf8.selected = true
    private val tfDisplayLinesLabel = new Label("Maximum number of errors to display:")
    private val tfDisplayLines = new TextField("2000", 7)
    private val tfMaxColumnCharsLabel = new Label("Maximum number of characters in a cell:")
    private val tfMaxColumnChars = new TextField("4096", 7)
    private val lblPathSubstitutions = new Label("Path Substitutions:")
    private val cbEnforceCaseSensitivePathChecks = new CheckBox("Enforce case-sensitive file path checks")
    cbEnforceCaseSensitivePathChecks.tooltip = "Performs additional checks to ensure that the case of file-paths in the CSV file match those of the filesystem"

    private def tablePathDialog(): Unit = {
      val csvFile = TextFile(Paths.get(txtCsvFile.getText), csvEncoding, validateUtf8)
      val schemaFile = TextFile(Paths.get(txtCsvSchemaFile.getText), csvSchemaEncoding)
      val expectedMaxCharsPerCell = convertTextboxValueToInt(maxCharsPerCell, "", _ => (), 4096)
      val identifierRows = CsvValidatorCmdApp.getColumnFromCsv(csvFile, schemaFile, "identifier", expectedMaxCharsPerCell).sorted

      val fromPath = identifierRows.headOption.getOrElse("").strip()

      val toPathField = new TextField(30)
      val fromPathField = new TextField(fromPath, 30)

      def pathToUri(path: Path) = {
        val uri = path.toUri.toString
        if (uri.endsWith("/")) uri else s"$uri/"
      }

      def updateFileText(path: Path): Try[String] = {
        toPathField.text = pathToUri(path)
        Success("Text updated")
      }

      val fileButton = new Button("Choose...")
      fileButton.reactions += {
        case ev: ButtonClicked =>
          val startingDir = if(toPathField.text.isEmpty) userDir.toFile else Path.of(toPathField.text).toFile
          val fromFolderName = Path.of(fromPathField.text).getFileName
          val helpText = s"Select the $fromFolderName folder"
          val fileChooser = new FileChooser(startingDir)
          fileChooser.multiSelectionEnabled = false
          fileChooser.title = helpText
          fileChooser.fileSelectionMode = SelectionMode.DirectoriesOnly
          chooseFileOrDir(fileChooser, f => updateFileText(f), fileButton, helpText)
      }

      val rows = List(
        Row("From", List(fromPathField)),
        Row("To", List(toPathField, fileButton))
      )
      addToTableDialog(parentFrame, "Add Path Substitution...", rows, tblPathSubstitutions.addRow)
    }

    private val tblPathSubstitutions = new Table(0, 2) {
      preferredViewportSize = new Dimension(500, 70)
      model = new DefaultTableModel(Array[Object]("From", "To"), 0)

      def addRow(rowData: Array[String]) : Unit = {
        model.asInstanceOf[DefaultTableModel].addRow(rowData.asInstanceOf[Array[AnyRef]])
      }

      def removeSelectedRows() : Unit = peer.getSelectedRows.zipWithIndex.foreach { case (row, index) =>
        model.asInstanceOf[DefaultTableModel]removeRow(row - index) // when you delete a row, the index of next row to delete shifts down by 1
      }

      def pathSubstitutions : List[(String, String)] = {
        for(rowIdx <- 0 to model.getRowCount - 1) yield (model.getValueAt(rowIdx, 0).asInstanceOf[String], model.getValueAt(rowIdx, 1).asInstanceOf[String])
      }.toList
    }

    private val spTblPathSubstitutions = new ScrollPane(tblPathSubstitutions)
    private val btnRemovePathSubstitution = new Button("Remove Path Substitution")
    private val btnAddPathSubstitution = new Button("Add Path Substitution...")

    btnRemovePathSubstitution.reactions += onClick(tblPathSubstitutions.removeSelectedRows())
    btnAddPathSubstitution.reactions += onClick(tablePathDialog())

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
      c.insets = new Insets(0, 0, 0, 0)
      layout(tfDisplayLinesLabel) = c

      c.gridx = 1
      c.gridy = 2
      c.insets = new Insets(0, 1, 0, 0)
      layout(tfDisplayLines) = c

      c.gridx = 0
      c.gridy = 3
      c.insets = new Insets(0, 0, 0, 0)
      layout(tfMaxColumnCharsLabel) = c

      c.gridx = 1
      c.gridy = 3
      c.insets = new Insets(0, 1, 0, 0)
      layout(tfMaxColumnChars) = c

      c.gridx = 0
      c.gridy = 4
      c.insets = new Insets(0,-7,0,0)
      layout(cbFailOnFirstError) = c

      c.gridx = 0
      c.gridy = 5
      c.insets = new Insets(0,-7,0,0)
      layout(cbEnforceCaseSensitivePathChecks) = c

      c.gridx = 0
      c.gridy = 6
      c.insets = new Insets(0,-7,0,0)
      layout(cbValidateUtf8) = c

      c.gridx = 0
      c.gridy = 7
      c.insets = new Insets(5,-2,0,0)
      layout(lblPathSubstitutions) = c

      c.gridx = 0
      c.gridy = 8
      c.gridwidth = 2
      c.insets = new Insets(0,-2,0,0)
      layout(spTblPathSubstitutions) = c

      c.gridx = 0
      c.gridy = 9
      c.anchor = Anchor.LineStart
      layout(btnRemovePathSubstitution) = c

      c.gridx = 1
      c.gridy = 9
      c.anchor = Anchor.LastLineEnd
      layout(btnAddPathSubstitution) = c
    }
    settingsGroup.add(settingsGroupLayout)

    add(settingsGroup)

    def csvEncoding: Charset = cmbCsvEncoding.selection.item
    def csvSchemaEncoding: Charset = cmbCsvSchemaEncoding.selection.item
    def numOfLinesToDisplay: String = tfDisplayLines.text.strip()
    def maxCharsPerCell: String = tfMaxColumnChars.text.strip()
    def failOnFirstError: Boolean = cbFailOnFirstError.selected
    def pathSubstitutions: List[(String, String)] = tblPathSubstitutions.pathSubstitutions
    def enforceCaseSensitivePathChecks: Boolean = cbEnforceCaseSensitivePathChecks.selected
    def validateUtf8 : Boolean = cbValidateUtf8.selected
  }
}
