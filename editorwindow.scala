// editorwindow.scala

package Editor

import java.awt.Color
import java.awt.Dimension
import java.awt.event.{KeyEvent, KeyListener, MouseEvent, MouseListener}
import java.awt.event.InputEvent._
import java.awt.Font
import java.io.File
import javax.swing._
import javax.swing.BorderFactory
import javax.swing.event.{ CaretEvent, CaretListener }
import javax.swing.JLabel
import javax.swing.ScrollPaneConstants._
import javax.swing.text.{TabSet, TabStop}
import javax.swing.text.SimpleAttributeSet
import javax.swing.text.StyleConstants
import scala.collection.mutable.Map
import Editor._

class WindowRange(var fromLine: Int, var toLine: Int, var fromOffset: Int) {
    // fromLine - the first line of the viewing range, starting at 0
    // toLine - the first line of the following part; -1 for 'to EOF'
    // fromOffset - the first char of the viewing range

    val BOF: Int = 0	// beginning of file
    val EOF: Int = -1	// end of file

    def containsLine(linenum: Int): Boolean = {
	if (toLine == EOF) {
	    if (fromLine <= linenum)
		true
	    else false
	}
	else {
	    if ((fromLine <= linenum) && (linenum < toLine))
		true
	    else false
	}
    }
    def insert(line: Int, str: String) {
	val num_EOLs = str.filter(_ == '\n').length
	if (line < fromLine) {
	    fromLine += num_EOLs
	    fromOffset += str.length
	}
	if ((toLine != EOF) && (line < toLine)) {
	    toLine += num_EOLs
	    if (toLine < 0)
		throw new Exception("file too big")
	}
    }
}

object WindowRangeFactory {
    def whole = new WindowRange(0, -1, 0)
    def partial(fromLine: Int, toLine: Int, fromOffset: Int) =
	new WindowRange(fromLine, toLine, fromOffset)
}

class EditorWindow(val textFile: TextFile, val windowRange: WindowRange)
    extends Box(BoxLayout.Y_AXIS)
{
    val myLightGray = new Color(240, 240, 240)
    // val myLightGray = Color.pink	// for development only
    val borderThickness = 1

    // tag line: icon and textfield
    var cleanDirtyLabel = new JLabel
    cleanDirtyLabel.setIcon(CleanIcon)
    var tagLineTextField = new JTextField
    tagLineTextField.setEditable(false)
    // val tagFontSize = tagLineTextField.getFont.getSize
    // tagLineTextField.setMinimumSize(
    //    new Dimension(0, tagFontSize))
    tagLineTextField.setMaximumSize(
        new Dimension(Integer.MAX_VALUE, 16))
    tagLineTextField.setBorder(BorderFactory.createEmptyBorder)
    tagLineTextField.setBackground(myLightGray)
    var tagLine: Box = Box.createHorizontalBox
    tagLine.add(cleanDirtyLabel)
    tagLine.add(tagLineTextField)

    // Set tagLineTextField text.
    val tagText: String = {
	val HOME: String = System.getenv("HOME")
	val f = new File(textFile.uri.getPath)
	val name = f.getName
	var parent = f.getParent
	if (parent.startsWith(HOME))
	    parent = "~" + parent.drop(HOME.length)
	name + " (" + parent + ") | "
    }
    tagLineTextField.setText(tagText)

    // text pane with listeners
    val textPane = new JTextPane
    textPane.setText(textFile.text.toString)
    textPane.setBorder(BorderFactory.createEmptyBorder)
    setTabstops
    textPane.addKeyListener(new editWindowKeyListener(this))
    textPane.getMouseListeners.foreach { textPane.removeMouseListener(_) }
    textPane.getMouseMotionListeners.foreach {
	textPane.removeMouseMotionListener(_)
    }
    textPane.addMouseListener(new editWindowMouseListener(this))

    // JScrollPane for textPane
    val jScrollPane = new JScrollPane(textPane,
	VERTICAL_SCROLLBAR_NEVER, HORIZONTAL_SCROLLBAR_NEVER)
    jScrollPane.setBorder(BorderFactory.createEmptyBorder)

    // scrollbar
    val scrollbarIcon = new ScrollBar(16, 16)
    val scrollbarLabel = new JLabel
    scrollbarLabel.setBackground(myLightGray)
    scrollbarLabel.setIcon(scrollbarIcon)

    // hbox for scrollbarLabel and jScrollPane
    val hbox = Box.createHorizontalBox
    hbox.setBackground(myLightGray)
    hbox.add(scrollbarLabel)
    hbox.add(jScrollPane)

    // VBox (== this) for tagLine and hbox
    this.add(tagLine)
    this.add(hbox)
    this.setBorder(BorderFactory.createLineBorder(
	Color.black, borderThickness))

    var currentLine = windowRange.fromLine
    // var currentCol = 0

    def foo(arg: Any) {
	showTextPane(None)
    }

    def getPreferredHeight: Int = {	// for showing the whole text
	val tagLineHeight = tagLine.getSize(null).getHeight.toInt
	val textPaneHeight = {
	    val lastCharRect = textPane.modelToView(textFile.text.length)
	    lastCharRect.y + lastCharRect.height
	}
	return tagLineHeight + textPaneHeight + 2 * borderThickness
    }
    def getViewportRows: Int = {
	val viewportHeight =
	    jScrollPane.getViewport.getSize(null).getHeight.toInt
	val fontHeight: Int = 
	    textPane.getFontMetrics(textPane.getFont).getHeight
	return viewportHeight / fontHeight
    }
    def redrawScrollbar {
	if (this.getComponentCount == 2) {	// textPane is shown.
	    // Temporally shrink scrollbar, to get jScrollPane's height.
	    var scrollbarIcon = new ScrollBar(16, 16)
	    scrollbarLabel.setIcon(scrollbarIcon)
	    UI_Manager.validate
	    val jScrollPaneHeight = jScrollPane.getSize(null).getHeight.toInt

	    scrollbarIcon = new ScrollBar(16, jScrollPaneHeight)
	    scrollbarLabel.setIcon(scrollbarIcon)
	}
    }
    def showTextPane(b: Option[Boolean]) {
	val textPaneIsHidden: Boolean = this.getComponentCount == 1
	b match {
	    case Some(true) if textPaneIsHidden => this.add(hbox)
	    case Some(false) if !textPaneIsHidden => this.remove(hbox)
	    case None =>	// toggle
		if (textPaneIsHidden)
		    this.add(hbox)
		else this.remove(hbox)
	    case _ => {}
	}
    }

    def insertText(line: Int, global_offset: Int, str: String) {
	// Called by TextFile.

	val local_offset = global_offset - windowRange.fromOffset

	printf("DEBUG EditorWindow::insertText(%d, %d, [%s]), range(%d, %d)\n",
	    line, global_offset, str, windowRange.fromLine, windowRange.toLine)
	if (windowRange.containsLine(line)) {
	    textPane.setCaretPosition(local_offset)
	    textPane.moveCaretPosition(local_offset)
	    textPane.replaceSelection(str)
	}
	windowRange.insert(line, str)
	targetColumn = None
    }
    def nextLine(dontcare: Any) {
	// TODO: tab
	// TODO: selection

	val wr_from = windowRange.fromOffset
	val local_offset = textPane.getCaretPosition
	val global_bol = textFile.getBOLOffset(wr_from + local_offset)
	val global_eol = textFile.getEOLOffset(wr_from + local_offset)
	val bol = global_bol - wr_from
	val eol = global_eol - wr_from
	val currentColumn = local_offset - bol	// zero-based
	val nextLineLength =	// excluding '\n'
	    textFile.getEOLOffset(global_eol + 1) - global_eol - 1
	val newColumn = targetColumn match {
	    case None => {
		targetColumn = Some(currentColumn)
		if (nextLineLength <= currentColumn)
		    nextLineLength
		else currentColumn
	    }
	    case Some(_) if nextLineLength <= currentColumn => nextLineLength
	    case Some(tcol) if tcol <= nextLineLength => tcol
	    case Some(_) => nextLineLength
	}
	var newOffset = eol + 1 + newColumn

	if (textFile.text.length < newOffset)
	    newOffset = textFile.text.length	// We shouldn't go past EOF.
	else currentLine += 1
	textPane.setCaretPosition(newOffset)
	textPane.moveCaretPosition(newOffset)
    }
    def setFocus = textPane.requestFocusInWindow
    def setCaret(local_offset: Int) {	// 'set' resets the selection.
	textPane.setCaretPosition(local_offset)
	textPane.moveCaretPosition(local_offset)
	selectionOtherEnd = None

	// Update targetColumn.
	val global_offset = windowRange.fromOffset + local_offset
	val col = global_offset - textFile.getBOLOffset(global_offset)
	targetColumn = Some(col)

	// Update currentLine.
	val num_EOLs =
	    textFile.text.toString.take(local_offset).filter(_ == '\n').length
	currentLine = windowRange.fromLine + num_EOLs

	printf(
	    "DEBUG EditorWindow setCaret(%d), new line %d, targetColumn %s\n",
	    local_offset, currentLine, targetColumn.toString)
    }

    def signalFontChange(arg: Any) = textFile.setMyFont(arg)
    def setMyFont(font: Font) = textPane.setFont(font)
    def setTabstops {
	val NUM_TABSTOPS = 16
	val EIGHT_SPACES = "        "
	val tabWidthInPix = UI_Manager.getFontMetrics(textFile.getFont)
	    .stringWidth(EIGHT_SPACES)
	var tabStops: Array[TabStop] = new Array(NUM_TABSTOPS)
	for (i <- 0 until NUM_TABSTOPS)
	    tabStops(i) = new TabStop((i + 1) * tabWidthInPix)
	var tabSet = new TabSet(tabStops)
	var attribs = new SimpleAttributeSet
	StyleConstants.setTabSet(attribs, tabSet)
	textPane.getStyledDocument.setParagraphAttributes(0,
	    textFile.text.length, attribs, false)
    }

    private var targetColumn: Option[Int] = None
    private var selectionOtherEnd: Option[Int] = None
}

class editWindowKeyListener(val editorWindow: EditorWindow)
    extends KeyListener
{
    // keymap for inputs without modifier
    val unmodifiedKeyMap: Map[Char, KeyEvent => Unit] = Map()
    val WORD_CHARS: String =
	"abcdefghijklmnopqrstuvwxyz" +
	"ABCDEFGHIJKLMNOPQRSTUVWXYZ" +
	"0123456789_"
    for (keyChar: Char <- WORD_CHARS)
	unmodifiedKeyMap += keyChar -> selfInsert _

    // keymap for inputs with CTRL
    val ctrlKeyMap: Map[String, KeyEvent => Unit] = Map(
	"N" -> nextLine _
    )

    def keyPressed(ev: KeyEvent) {
	val keyText = KeyEvent.getKeyText(ev.getKeyCode)
	if ((ev.getModifiers() & CTRL_MASK) != 0) {
	    printf("keyPressed(): keyCode [%d], keyText[%s] with Ctrl\n",
		ev.getKeyCode, keyText)
	    if (ctrlKeyMap.contains(keyText))
		ctrlKeyMap(keyText)(ev)
	}
	else {	// without modifier
	    if (unmodifiedKeyMap.contains(ev.getKeyChar))
		unmodifiedKeyMap(ev.getKeyChar)(ev)
	}
	ev.consume
    }
    def keyReleased(ev: KeyEvent) = ev.consume
    def keyTyped(ev: KeyEvent) = ev.consume

    def nextLine(ev: KeyEvent): Unit =
	editorWindow.nextLine(null)
    def selfInsert(ev: KeyEvent): Unit = {
	// This doesn't modify editorWindow directly.
	// Instead, it will be updated by TextFile.

	// TODO: selection

	val textPane = ev.getComponent.asInstanceOf[JTextPane]
	editorWindow.textFile.insertText(
	    editorWindow.currentLine,
	    editorWindow.windowRange.fromOffset + textPane.getCaretPosition,
	    ev.getKeyChar.toString)
    }
}

class editWindowMouseListener(val editorWindow: EditorWindow)
    extends MouseListener
{
    def mouseClicked(ev: MouseEvent) {
	val local_offset =
	    editorWindow.textPane.viewToModel(ev.getPoint)
	ev.getButton match {
	    case 1 if ev.getClickCount == 1 => {
		printf("mouseClicked: button 1, count 1, offset %d\n",
		    local_offset)
		editorWindow.setCaret(local_offset)
		editorWindow.setFocus
	    }
	    case 1 => printf("mouseClicked: button 1, count 2+, offset %d\n",
		local_offset)
	    case 2 => printf("mouseClicked: button 2, offset %d\n",
		local_offset)
	    case 3 => printf("mouseClicked: button 3, offset %d\n",
		local_offset)
	    case _ => {}
	}
    }
    def mouseEntered(ev: MouseEvent) {}
    def mouseExited(ev: MouseEvent) {}
    def mousePressed(ev: MouseEvent) {}
    def mouseReleased(ev: MouseEvent) {}
}

class ScrollBar(width: Int, height: Int) extends Icon {
    private val w = if (width < 8) 8 else width
    private val h = if (height < 8) 8 else height
    def getIconHeight: Int = h
    def getIconWidth: Int = w
    def paintIcon(c: java.awt.Component, g: java.awt.Graphics,
	    x: Int, y: Int): Unit = {
	// g.setColor(Color.pink)
	g.setColor(new Color(240, 240, 240))
	g.fillRect(x, y, w, h)
	g.setColor(Color.green)
	g.drawRect(x + 1, y + 1, w - 3, h - 3)
    }
}

object CleanIcon extends Icon {
    def getIconHeight: Int = 16
    def getIconWidth: Int = 16
    def paintIcon(c: java.awt.Component, g: java.awt.Graphics,
	    x: Int, y: Int): Unit = {
	// g.setColor(Color.pink)
	g.setColor(new Color(240, 240, 240))
	g.fillRect(x, y, 16, 16)
	g.setColor(Color.black)
	g.drawRect(x + 1, y + 1, 13, 13)
    }
}

object DirtyIcon extends Icon {
    def getIconHeight: Int = 16
    def getIconWidth: Int = 16
    def paintIcon(c: java.awt.Component, g: java.awt.Graphics,
	    x: Int, y: Int): Unit = {
	// g.setColor(Color.pink)
	g.setColor(new Color(240, 240, 240))
	g.fillRect(x, y, 16, 16)
	g.setColor(Color.black)
	g.fillRect(x + 1, y + 1, 13, 13)
    }
}

// eof
