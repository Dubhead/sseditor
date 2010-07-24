// buffer.scala

package Editor

import java.io._
import java.io.File._
import java.net.URI
import java.awt.Font
import scala.collection.mutable.Map

import Editor._

abstract class TextFile {
    // EditorWindow instances referring to this
    var editorWindows: Array[EditorWindow] = Array()

    val uri: URI
    var dirty: Boolean = false	// contains unsaved changes?
    var lastModified: Long = 0
    var substantial: Boolean = false	// actually exists on FS?

    def lastModifiedOnFS(): Long
    def length: Int
    var text = new StringBuilder()
    def addEditorWindow(ew: EditorWindow) {
	if (!(editorWindows.contains(ew)))
	    editorWindows ++= Array(ew)
    }
    def deleteEditorWindow(ew: EditorWindow) {
	editorWindows = editorWindows.filter(_ != ew)
	    .asInstanceOf[Array[EditorWindow]]
    }
    def insertText(line: Int, offset: Int, str: String) {
	printf("DEBUG insertText(%d, %d, [%s])\n", line, offset, str)

	text.insert(offset, str)
	if (!dirty) dirty = true
	editorWindows.foreach(_.insertText(line, offset, str))
    }

    def getLineNumber(offset: Int): Int =
	text.substring(0, offset).toList.count(_ == '\n')
    def getBOLOffset(offset: Int): Int = {	// beginning of line
	var bol = offset
	while ((bol > 0) && (text.charAt(bol - 1) != '\n'))
	    bol -= 1
	bol
    }
    def getEOLOffset(offset: Int): Int = {	// '\n' in this line
	var eol = offset
	while ((eol < text.length) && (text.charAt(eol) != '\n'))
	    eol += 1
	eol
    }

    //// fonts ////
    private var fontFamily: String = UI_Manager.getDefaultFonts._1
    private var fontSize: Int = UI_Manager.getDefaultFonts._3
    private var font = new Font(fontFamily, Font.PLAIN, fontSize)
    def setMyFont(arg: Any) {
	val (defFont, altFont, defSize) = UI_Manager.getDefaultFonts
	arg match {
	    case size: Int => { fontSize = size }
	    case name: String => { fontFamily = name }
	    case null =>
		fontFamily = if (fontFamily == defFont) altFont else defFont
	    case _ => throw new Exception
	}
	font = new Font(fontFamily, Font.PLAIN, fontSize)
	for (ew <- editorWindows) {
	    ew.setMyFont(font)
	    ew.setTabstops
	}
    }
    def getFont = font
}

class LocalTextFile(path: String) extends TextFile {
    private val file = new File(path)
    if (!file.exists) {
    }
    else if (file.isDirectory) {
	// TODO: Create a directory buffer.
	throw new Exception("directory: " + path)
    }
    else if (!file.isFile) {
	throw new Exception("not a file: " + path)
    }
    else if (!file.canRead) {
	throw new Exception("cannot read: " + path)
    }
    else {	// regular, readable file
	// Read the file content.
	text = new StringBuilder(file.length.asInstanceOf[Int])
	val br = new BufferedReader(new FileReader(file))
	var line: String = br.readLine
	while (line != null) {
	    text.append(line)
	    text.append("\n")
	    line = br.readLine
	}

	lastModified = file.lastModified
	substantial = true
    }

    val uri = file.toURI
    def lastModifiedOnFS(): Long = file.lastModified
    def length: Int = text.length
}

object TextFileFactory {
    // TODO: Accept other URIs like sftp://
    def create(path: String): TextFile = {
	val path1 = new File(path) getCanonicalPath
	val result: TextFile = new LocalTextFile(path1)
	return result
    }
}

object TextFileManager {
    def get(uri: URI): TextFile =
	if (textFiles.contains(uri))
	    textFiles(uri)
	else {
	    val tf = TextFileFactory.create(uri.getPath)
	    textFiles += uri -> tf
	    tf
	}

    private val textFiles: Map[URI, TextFile] = Map()
}

// eof
