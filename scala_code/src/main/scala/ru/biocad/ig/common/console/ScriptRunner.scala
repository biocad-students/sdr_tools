package ru.biocad.ig.common.console

import com.typesafe.scalalogging.slf4j.LazyLogging
import scala.sys.process._
import mustache._
import scala.io.Source

import ru.biocad.ig.common.io.common.SourceReader


/** This class/object (i'm not completely sure what it will be) loads console commands
  * from settings file (each console command represented as single line with mustache-style argument placeholders),
  * performs substitution of environment variables in commands with their values, and executes that commands.
  *
  * Commands preprocessing is performed in underlying object [[ru.biocad.ig.common.console.ScriptPreprocessor]]. Constructor arguments are the same as for that class.
  *
  * The only reason why it can be found here - Pavel Andreevich didn't told me what should I use to run such commands
  * (I know there is some code, but I don't have access to it).
  * Still I have to add postpocessing stage, that's why I wrote this class. Feel free to use something else.
  *
  * @param tasksSource contains a sequence of console commands (1 per line) with mustache-styled placeholders
  */
class ScriptRunner(val tasksSource : Source) extends ScriptPreprocessor(tasksSource) with LazyLogging {

  /** runs task sequence with params given as method argument
    *
    * @param values contains values to merge
    * Simply runs commands, and then prints error\output to console
    */
  def run(values : Map[String, String]) = {
    val preparedCommands = substitute(values)

    val scriptLogger = ProcessLogger(println(_), println(_))
    preparedCommands.zipWithIndex.foreach({
      case (templateCmd, index) => {
        logger.debug("processing command %d: ".format(index))
        templateCmd ! scriptLogger
      }})
  }

}
