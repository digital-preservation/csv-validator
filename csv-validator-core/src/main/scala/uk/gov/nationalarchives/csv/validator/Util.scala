/**
 * Copyright (c) 2013, The National Archives <digitalpreservation@nationalarchives.gov.uk>
 * http://www.nationalarchives.gov.uk
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package uk.gov.nationalarchives.csv.validator

import scalax.file.Path
import scalaz._
import Scalaz._
import java.util.regex.{Matcher, Pattern}
import java.net.URI
import scala.util.Try
import java.io.{FileNotFoundException, FilenameFilter, File}
import java.net.URLDecoder
import scala.annotation.tailrec


object Util {

  type AppValidation[S] = ValidationNel[FailMessage, S]

  def checkFilesReadable(files: List[Path]) = files.map(fileReadable).sequence[AppValidation, FailMessage]

  def fileReadable(file: Path): AppValidation[FailMessage] = if (file.exists && file.canRead) SchemaMessage(file.path).successNel[FailMessage] else fileNotReadableMessage(file).failureNel[FailMessage]

  def fileNotReadableMessage(file: Path) = SchemaMessage("Unable to read file : " + file.path)

  /**
   * Check if the list l1 contain all element in l2
   * @param l1 the containing list
   * @param l2 the contained list
   * @tparam A type of the list
   * @return true,if the list l1 contain all element in l2
   */
  def containAll[A](l1: List[A], l2: List[A]): Boolean =
    l2 forall  ((l1.toSet) contains)

  /**
   * List recursively all the files (but not the subfolder) in the folder given as a parameter
   * @param folder
   * @return List of all filename
   */
  def findAllFiles(includeFolder : Boolean,folder: File): Set[File] = {
    if (folder.exists()){
      val these = folder.listFiles.toSet
      val head = if (includeFolder) Set(folder) else Nil
      (head ++ these.filter( f => if (!includeFolder) f.isFile else true) ++ these.filter(f => f.isDirectory && !(f.getName == "RECYCLER") && !(f.getName == "$RECYCLE.BIN")).flatMap(file => findAllFiles(includeFolder, file))).toSet
    }
    else
      throw new FileNotFoundException(s"Cannot find the folder $folder")
  }



  abstract class TypedPath {

    def path : String

    val separator = UNIX_FILE_SEPARATOR
    //private lazy val p = Path.fromString(path)

    def name = path.lastIndexOf(separator) match {
      case -1 =>
        path
      case index =>
        path.substring(index + 1)
    }
    //def hasParent = !p.parent.isEmpty
    //protected def parentPath = p.parent.get.path
    def hasParent = parentPath.nonEmpty
    private lazy val parentPath : Option[String] = {
      path.lastIndexOf(separator) match {
        case -1 =>
          None
        case index =>
          Some(path.substring(0, index))
      }
    }
    def parent : Option[TypedPath] = parentPath match {
      case Some(pp) => Some(construct(pp))
      case _ => None
    }
    protected def construct(p: String) : TypedPath
    def thisFolder : TypedPath = construct(".")
    def toString : String
    protected lazy val isWindowsPlatform = sys.props("os.name").toLowerCase.startsWith("win")
    def toPlatform : TypedPath
  }

  object TypedPath {

    //extracts the scheme from the file:/c:/ or file:/// or file:///c:/ part of a URI
    lazy val fileUriPattern = Pattern.compile("((file:///)[a-zA-Z]+:/|(file://)/|(file:/)[a-zA-Z]+:/)(.*)")
    //extracts the path from the URI
    lazy val fileUriPathPattern = Pattern.compile("file:///([a-zA-Z]+:/.*)|file://(/.*)|file:/([a-zA-Z]+:/.*)")

    def apply(path : String) : TypedPath = {
      val fileUriMatcher = fileUriPattern.matcher(path)
      if(fileUriMatcher.matches) {
        val fileUriPathMatcher = fileUriPathPattern.matcher(path)
        new FileUriPath(fileUriMatcher.replaceFirst("$2$3$4"), fileUriPathMatcher.replaceFirst("$1$2$3"))
      } else if(path.contains(UNIX_FILE_SEPARATOR) && !path.contains(WINDOWS_FILE_SEPARATOR)) {
        new UnixPath(path)
      } else {
        new WindowsPath(path)
      }
    }
  }

  case class FileUriPath(uriPrefix : String, path: String) extends TypedPath {
    def toURI : URI = new URI(uriPrefix + FileSystem.file2PlatformIndependent(path))
    override def toString : String = toURI.toString
    override protected def construct(p : String) = new FileUriPath(uriPrefix, p);
    override def toPlatform = this
  }
  case class WindowsPath(path : String) extends TypedPath {
    override val separator = WINDOWS_FILE_SEPARATOR
    override def toString = FileSystem.file2WindowsPlatform(path)
    override protected def construct(p : String) = new WindowsPath(p)
    override def toPlatform = if(isWindowsPlatform) this else new UnixPath(FileSystem.file2UnixPlatform(path))
  }
  case class UnixPath(path : String) extends TypedPath {
    override def toString = FileSystem.file2UnixPlatform(path)
    override protected def construct(p: String) = new UnixPath(p)
    override def toPlatform = if(isWindowsPlatform) new WindowsPath(FileSystem.file2WindowsPlatform(path)) else this
  }


  object FileSystem {
    def createFile(filename: String): Try[File] =  Try{ if( filename.startsWith("file:")) new File( new URI(file2PlatformIndependent(filename))) else  new File( URLDecoder.decode(filename, ENCODING.name()) )} //TODO encoding shudld not be hard-coded here. Should be determined from cmd-line/ui user setting

    def replaceSpaces(file: String): String = file.replace(" ", "%20")

    def file2PlatformIndependent(file: String): String =
      file.replaceAll("""([^\\]?)\\([^\\]?)""", "$1/$2")

    def file2UnixPlatform(file: String) : String = file2PlatformIndependent(file)

    def file2WindowsPlatform(file: String) : String =
      file.replaceAll("([^/]?)/([^/]?)", """$1\\$2""")

    def convertPath2Platform(filename: String): String = {
      if ( filename.startsWith("file:/"))  replaceSpaces(filename) else file2PlatformIndependent( filename )
    }

    def file2PatformDependent(file: String) : String = TypedPath(file).toPlatform.toString

    /**
     * Checks that a filepath exactly matches
     * the file path available on disk
     *
     * This ensures case-sensitivity
     * and is useful on platforms such as
     * Windows NTFS which are case-insensitive,
     * where new File("test.txt").exists
     * and new File("TEST.TXT").exists
     * may both return true when there is
     * only one file.
     */
    @tailrec
    final def caseSensitivePathMatchesFs(f: File): Boolean = {

      val parent = Option(f.getAbsoluteFile.getParentFile)
      parent match {
        case None =>
          true //used to exit

        case Some(p) =>
          val foundChild = p.list(new FilenameFilter {
            def accept(dir: File, name: String): Boolean = name == f.getName
          })
          foundChild.nonEmpty && caseSensitivePathMatchesFs(p)
      }
    }
  }

  case class FileSystem(basePath: Option[String], file: String, pathSubstitutions: List[(String,String)] ) {

    def this( root:String, file: String, pathSubstitutions: List[(String,String)] ) = this( Some(root), file, pathSubstitutions)

    def this( file: String, pathSubstitutions: List[(String,String)]) = this(None, file, pathSubstitutions)

    val separator: Char = FILE_SEPARATOR

    private def substitutePath(filename: String): String = {
      val x = {
        pathSubstitutions.filter {
          case (subFrom, _) => filename.contains(subFrom)
        }.map {
          case (subFrom, subTo) => filename.replaceFirst(Matcher.quoteReplacement(subFrom), Matcher.quoteReplacement(FileSystem.file2PlatformIndependent(subTo)))
        }
      }
      if(x.isEmpty)
        filename
      else
        x.head
    }

    def jointPath: String = {
      val uri_sep: Char = URI_PATH_SEPARATOR

      basePath match {
        case Some(bp) =>

          if (bp.length > 0 && bp.last != uri_sep && file.head != uri_sep) {
            bp + uri_sep + file
          } else if (bp.length > 0 && bp.last == uri_sep && file.head == uri_sep) {
            bp + file.tail
          } else {
            bp + file
          }

        case None => file
      }
    }

    def exists(enforceCaseSensitivePathChecks: Boolean = false): Boolean = {
      FileSystem.createFile(FileSystem.convertPath2Platform(substitutePath(jointPath))) match {
        case scala.util.Success(f) => {
          val exists = f.exists
          if(exists && enforceCaseSensitivePathChecks) {
            FileSystem.caseSensitivePathMatchesFs(f)
          } else {
            exists
          }
        }
        case scala.util.Failure(_) => false
      }
    }

    def expandBasePath: String = {
      if( basePath.isEmpty || basePath.getOrElse("") == "")
        FileSystem.file2PlatformIndependent(substitutePath(file))
      else
        FileSystem.file2PlatformIndependent(substitutePath(jointPath))
    }
  }
}
