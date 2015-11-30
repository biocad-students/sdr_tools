package ru.biocad.ig.common.console

import com.typesafe.scalalogging.slf4j.LazyLogging
import scala.sys.process._
import mustache._
import scala.io.Source

import ru.biocad.ig.common.io.common.SourceReader


/** This class/object (i'm not completely sure what it will be) loads console commands
  * from settings file (each console command represented as single line with mustache-style argument placeholders),
  * and substitutes values for mustache variables and environment variables.
  * Environment variables are defined as a sequence of characters starting with $, consisting of uppercase letters, digits or _ sign.
  *
  * @param tasksSource contains a sequence of console commands (1 per line) with mustache-styled placeholders and (optionally) variable names from environment
  *
  * I decided to move String processing part from [[ru.biocad.ig.common.console.ScriptRunner]] to this class to use it not only for command lines execution, but for preparation of configuration files from templates.
  */
class ScriptPreprocessor(val templateSource : Source) extends LazyLogging {
  private val templateCommands = templateSource.getLines().filterNot(_.startsWith("#")).map({ cmd => new Mustache(cmd) })

  def this(tasksFile : String) = this(Source.fromFile(tasksFile))

  private val envVar = """\$([A-Z_0-9]+)""".r.unanchored

  /** For given templateCommands, performs substitution of mustache keys in `values` Map and environment variables with their values.
    *
    * @param values contains values to merge it to tasks in list
    * @param processEnvVars indicates whether to perform substitution of environment variables with their values, or not.
    * @return Iterator with processed strings
    */
  def substitute(values : Map[String, String], processEnvVars : Boolean = true) = {
    templateCommands.zipWithIndex.map({
      case (templateCmd, index) => {
        val commandWithMustacheVars = templateCmd.render(values)
        logger.debug("processing command %d: ".format(index))
        logger.debug(commandWithMustacheVars)
        if (processEnvVars) {
          val commandWithVars = envVar.replaceAllIn(commandWithMustacheVars,
              m => sys.env.get(m.group(1)) match {
                  case Some(command) => command
                  case None => "$" + m.group(1) //keeps the variable in script if not present in environment
              })
          logger.debug(commandWithVars)
          commandWithVars
        } else {
          commandWithMustacheVars
        }
      }})
  }
}
