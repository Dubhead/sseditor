// command.scala

package Editor

case class BaseCommand(
    range: Option[String], cmdname: String, arg: Option[String]) {}

object CommandParser extends Object {
    def parse(raw_commandline: String): BaseCommand = {
	val commandline = raw_commandline.trim
	val i_sp = commandline.indexOf(" ")
	val range = None	// TODO
	val cmdname: String =
	    if (i_sp == -1) commandline else commandline.take(i_sp)
	val arg: Option[String] =
	    if (i_sp == -1) None else Some(commandline.drop(i_sp + 1))

	new BaseCommand(range, cmdname, arg)
    }
}

// eof
