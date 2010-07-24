// engine.scala

package Editor

import java.io.File
import java.lang.Integer.decode;
import java.net.URI

import Editor._

object Engine {
    def send(command: BaseCommand): Unit = command match {
	case BaseCommand(_, "foo", arg) =>
	    foo(arg)
	case BaseCommand(None, "e", Some(path)) =>
	    printf("command [e], path [%s]\n", path)
	    openFile(System.getProperty("user.dir"), path)
	case BaseCommand(None, "q", _) =>
	    quit()
	case BaseCommand(None, "font", arg) =>
	    setTextPaneFont(arg)
	case BaseCommand(_, cmdname, None) =>
	    printf("unknown command [%s]\n", cmdname)
	case BaseCommand(_, cmdname, Some(arg)) =>
	    printf("unknown command [%s][%s]\n", cmdname, arg)
    }

    def foo(arg: Option[String]) = UI_Manager.foo(arg)

    def openFile(cwd: String, path: String) {
	// TODO: Check if UI_Manager already has it.

	// Join cwd and path.
	// Note: Redundant "/" will be removed by getCanonicalPath.
	val p: String =
	    if (path.startsWith("~"))
		System.getenv("HOME") + File.separator + path.drop(1)
	    else if (path.startsWith(File.separator))
		path
	    else
		cwd + File.separator + path

	val uri: URI = new File((new File(p)) getCanonicalPath).toURI
	val tf = TextFileManager.get(uri)
	val editorWindow = UI_Manager.postNewWindow(tf);
	editorWindow.redrawScrollbar
	UI_Manager.setMyTitle(uri)
    }

    def quit() {
	printf("quit()\n")
	System.exit(0)
    }

    def setTextPaneFont(arg: Option[String]) = arg match {
	case None => UI_Manager.setTextPaneFont(null)
	case Some(arg) =>
	    try {
		val fontSize: Int = Integer.parseInt(arg)
		UI_Manager.setTextPaneFont(fontSize)
	    }
	    catch {
		case _: NumberFormatException => {
		    UI_Manager.setTextPaneFont(arg)
		}
	    }
    }

    def main(args: Array[String]): Unit = {
	val c: BaseCommand = CommandParser.parse(args(0))
	this.send(c)
    }

}

// eof
