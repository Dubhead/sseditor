// ui_manager.scala

package Editor

import java.awt.Color
import java.awt.Dimension
import java.awt.event.{ActionEvent, ActionListener,
    HierarchyEvent, HierarchyBoundsAdapter,
    KeyEvent, KeyListener}
import java.awt.event.InputEvent._
import java.awt.Font
import java.io.File
import java.net.URI
import javax.swing._
import javax.swing.Box
import javax.swing.BorderFactory
import javax.swing.ScrollPaneConstants._
import Editor._

object UI_Manager extends JFrame("エディタ (仮)") {
    val APP_NAME: String = "エディタ (仮)"
    val DEFAULT_FONT_FAMILY: String = "Sans Serif"
    val ALTERNATIVE_FONT_FAMILY: String = "Monospaced"
    val DEFAULT_FONT_SIZE: Int = 12
    val HOME: String = System.getenv("HOME")

    def main(args: Array[String]) {
	this.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)

	pane = this.getContentPane.asInstanceOf[JComponent]
	pane.setLayout(new BoxLayout(pane, BoxLayout.X_AXIS))
	pane.setBackground(Color.white)
	pane.addHierarchyBoundsListener(new ResizeListener)

	val tempTextFile = TextFileFactory.create("/home/miura/work/foo.txt")
	val editorWindow = new EditorWindow(tempTextFile,
	    WindowRangeFactory.whole)
	tempTextFile.addEditorWindow(editorWindow)

	// command line (仮)
	var tf = new JTextField
	val fontSize = tf.getFont.getSize
	tf.setMaximumSize(new Dimension(Integer.MAX_VALUE, fontSize))
	tf.setBorder(BorderFactory.createEmptyBorder)
	tf.addActionListener(new commandlineActionListener)

	// a VBox for a column
	var columnPanel0 = Box.createVerticalBox
	columnPanel0.add(editorWindow)
	columnPanel0.add(Box.createVerticalGlue)
	columnPanel0.add(tf)

	columnPanels = Array(columnPanel0)
	pane.add(columnPanel0)
	// this.setMinimumSize(new Dimension(300, 200))
	this.setMinimumSize(new Dimension(584, 200))
	this.setVisible(true)
    }
    def foo(arg: Any) {
	columnPanels(0).getComponent(0).asInstanceOf[EditorWindow].foo(arg)
	UI_Manager.validate
	val ew = columnPanels(0).getComponent(0).asInstanceOf[EditorWindow]
	printf("DEBUG current height %d, getPreferredHeight %d, rows %d\n",
	    ew.getHeight.toInt, ew.getPreferredHeight, ew.getViewportRows)

	// val tempTextFile = TextFileFactory.create("/home/miura/work/bar.txt")
	// val tempTextWindow = new EditorWindow(tempTextFile,
	//     WindowRangeFactory.whole)
	// tempTextFile.addEditorWindow(tempTextWindow)
	// val col: Box = columnPanels(0)
	// col.add(tempTextWindow, 1)
	// tempTextWindow.setFocus
	// tempTextWindow.setCaret(0)
	// //
	// val colWidth = col.getSize(null).getWidth.toInt
	// val colHeight = col.getSize(null).getHeight.toInt
	// col.getComponent(0).setPreferredSize(
	//     new Dimension(colWidth, (colHeight - 16) / 2))
	// col.getComponent(1).setPreferredSize(
	//     new Dimension(colWidth, (colHeight - 16) / 2))
	// //
	// UI_Manager.validate
	// col.getComponent(0).asInstanceOf[EditorWindow].redrawScrollbar
	// col.getComponent(1).asInstanceOf[EditorWindow].redrawScrollbar
	// //
	// printf("DEBUG ui-foo %s\n", columnPanels(0).toString)
	// for (w <- columnPanels(0).getComponents) {
	//     printf("\t%s\n", w.toString)
	// }
    }
    def postNewWindow(tf: TextFile): EditorWindow = {
	val editorWindow = new EditorWindow(tf, WindowRangeFactory.whole)
	tf.addEditorWindow(editorWindow)
	val col: Box = columnPanels(0)
	col.remove(0)
	col.add(editorWindow, 0)
	editorWindow.setFocus
	editorWindow.setCaret(0)
	this.validate

	return editorWindow
    }
    def resizeCallback {
	for (col <- columnPanels; w <- col.getComponents) w match {
	    case ew: EditorWindow => ew.redrawScrollbar
	    case _ => {}
	}
	this.validate
    }

    // def getcwd: String
    // def postMessage(msg: String)
    // def debugLog(lazy msg: String)
    def setMyTitle(title: Any) = title match {
	case uri: URI => {
	    val f = new File(uri.getPath)
	    val name = f.getName
	    var parent = f.getParent
	    if (parent.startsWith(HOME))
		parent = "~" + parent.drop(HOME.length)
	    super.setTitle(name + " (" + parent + ") - " + APP_NAME)
	}
	case msg: String => super.setTitle(msg)
	case _ => {}
    }
    def setTextPaneFont(arg: Any) {
	columnPanels(0).getComponent(0).asInstanceOf[EditorWindow]
	    .signalFontChange(arg)
    }
    def setDefaultFonts(defaultFontFamily: Option[String],
	    altFontFamily: Option[String], defaultSize: Option[Int]) {
	defaultFontFamily match {
	    case Some(ff) => default_font_family = ff
	    case _ => {}
	}
	altFontFamily match {
	    case Some(af) => alternative_font_family = af
	    case _ => {}
	}
	defaultSize match {
	    case Some(s) => default_font_size = s
	    case _ => {}
	}
    }
    def getDefaultFonts: Tuple3[String, String, Int] =
	(default_font_family, alternative_font_family, default_font_size)

    private var pane: java.awt.Container = _
    private var columnPanels: Array[Box] = _
    private var default_font_family = DEFAULT_FONT_FAMILY
    private var alternative_font_family = ALTERNATIVE_FONT_FAMILY
    private var default_font_size = DEFAULT_FONT_SIZE
    private var debugLogEnabled = true

}

// for command line (仮)
class commandlineActionListener extends ActionListener {
    def actionPerformed(ev: ActionEvent) {
	val tf = ev.getSource.asInstanceOf[JTextField]
	val text = tf.getText
	val command = CommandParser.parse(text)
	Engine.send(command)
    }
}

class ResizeListener extends HierarchyBoundsAdapter {
    override def ancestorResized(ev: HierarchyEvent) {
	UI_Manager.resizeCallback
    }
}

// eof
