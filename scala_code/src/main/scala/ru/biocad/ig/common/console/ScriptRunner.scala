package ru.biocad.ig.common.console

import scala.sys.process._
import mustache._
import scala.io.Source
import ru.biocad.ig.common.io.common.SourceReader

/** This class/object (i'm not completely sure what it will be) loads console commands
  * from settings file (each console command represented as single line with mustache-style argument placeholders),
  * executes that commands.
  *
  * The only reason why it can be found here - Pavel Andreevich didn't told me what should I use to run such commands
  * (I know there is some code, but I don't have access to it).
  * Still I have to add postpocessing stage, that's why I wrote this class. Feel free to use something else.
  *
  * @param tasksSource contains a sequence of console commands (1 per line) with mustache-styled placeholders
  */
class ScriptRunner(val tasksSource : Source) {
  private val templateCommands = tasksSource.getLines().map({cmd => new Mustache(cmd) })

  def this(tasksFile : String) = this(Source.fromFile(tasksFile))

  /** runs task sequence from tasksFile with params given as method argument
    *
    * @param values contains values to merge it to tasks in list
    */
  def run(values : Map[String, String]) = {
    //val _stdout = StringBuilder.newBuilder
    //val _stderr = StringBuilder.newBuilder

    val scriptLogger = ProcessLogger(println(_), println(_))
    templateCommands.foreach({templateCmd => templateCmd.render(values) ! scriptLogger })
  }

}
