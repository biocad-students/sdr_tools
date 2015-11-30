package ru.biocad.ig.common.console

import com.typesafe.scalalogging.slf4j.LazyLogging
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
class ScriptPreprocessor(val templateSource : Source) extends LazyLogging {
  private val templateCommands = templateSource.getLines().filterNot(_.startsWith("#")).map({ cmd => new Mustache(cmd) })

  def this(tasksFile : String) = this(Source.fromFile(tasksFile))

  private val envVar = """\$([A-Z_0-9]+)""".r.unanchored

  /** runs task sequence from tasksFile with params given as method argument
    *
    * @param values contains values to merge it to tasks in list
    */
  def substitute(values : Map[String, String]) = {

    templateCommands.zipWithIndex.map({
      case (templateCmd, index) => {
        val commandWithMustacheVars = templateCmd.render(values)
        logger.info("processing command %d: ".format(index))
        logger.info(commandWithMustacheVars)
        val commandWithVars = envVar.replaceAllIn(commandWithMustacheVars, m => sys.env(m.group(1)))
        logger.info(commandWithVars)
        commandWithVars
      }})
  }

}
