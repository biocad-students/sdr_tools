package test.alphabet

//import DefaultJsonProtocol._
import scopt.OptionParser
import java.io.{File, PrintWriter}
import scala.io.Source
import com.typesafe.scalalogging.slf4j.LazyLogging


import ru.biocad.ig.common.algorithms.MonteCarloRunner
import ru.biocad.ig.common.console._

/** Selects method of data processing from command line parameters
  * should have several ways to invoke:
  * 1. input file in pdb format with structure to refine/fold
  * 2. input file in pdb to perform alanine scanning
  * 3. input file in fasta or as a sequence of aminoacids - to compare with known folding
  */
object MCTest extends LazyLogging {

  private case class Config(inputFile : File = new File("2OSL.pdb"),
      outputFile : File = new File("result.pdb"),
      mcTimeUnits : Int = 50,
      mode : String = "fold",
      sequence : String = "GARFIELD",//"RMAQLEAKVEELLSKNWNLENEVARLKKLVGER",//
      //TODO: add option - refine/fold/alascan - default refine
      postprocess : Boolean = false,
      settingsFile : File = new File("config/lattice_params.json"),
      debug : Boolean = false)

  private def getParser = new OptionParser[Config]("sdr_tools") {
      opt[Int]('n', "mcTimeUnits") action {(s, c) =>
        c.copy(mcTimeUnits = s)} text "number of iterations (time units -  each time unit contains several move attempts) to call MC"
      opt[File]('o', "outputFile") valueName("<file>") action {(s, c) =>
        c.copy(outputFile = s)} text "output file in PDB format"
      cmd("refine") action {(_, c) =>
        c.copy(mode = "refine") } text("starts structure refinement procedure for given input PDB file.") children(
          opt[File]('i', "inputFile") valueName("<file>") action {(s, c) =>
            c.copy(inputFile = s)} text "input file in PDB format"
        )
      cmd("fold") action {(_, c) =>
        c.copy(mode = "fold")} text("folds protein with given aminoacid sequence.") children(
          opt[String]('s', "sequence") action {(s, c) =>
            c.copy(sequence = s)} text("aminoacid sequence to process")
        )
      cmd("scan") action {(_, c) =>
        c.copy(mode = "scan") } text("performs alanine scanning for given input pdb file") children(
          opt[File]('i', "inputFile") valueName("<file>") action {(s, c) =>
            c.copy(inputFile = s)} text "input file in PDB format"
        )
      cmd("recreate") action {(_, c) =>
        c.copy(mode = "recreate") } text("performs structure fitting to chain and restores full-atom structure (with no MC runs)") children(
          opt[File]('i', "inputFile") valueName("<file>") action {(s, c) =>
            c.copy(inputFile = s)} text "input file in PDB format"
        )
      opt[File]("settingsFile") valueName("<file>") action {(s, c) =>
          c.copy(settingsFile = s)} text "settings main file name"
      opt[Unit]("postprocess") action {(_, c) => c.copy(postprocess = true)} text "call postprocessing script"
      opt[Unit]("debug") action {(_, c) => c.copy(debug = true)} text "enable debug output"
      help("help") text "this message"
  }

  def main(args : Array[String]) : Unit = {
    val parser = getParser
    parser.parse(args, Config()) match {
      case Some(config) =>
        try {
          config.mode match {
            case "fold" => {
              MonteCarloRunner(config.settingsFile).fold(config.sequence, config.mcTimeUnits, config.outputFile)
            }
            case "refine" => {
              MonteCarloRunner(config.settingsFile).refine(config.inputFile, config.mcTimeUnits, config.outputFile)
            }
            case "scan" => {
              MonteCarloRunner(config.settingsFile).scan(config.inputFile, config.mcTimeUnits, config.outputFile)
            }
            case "recreate" => {
               MonteCarloRunner(config.settingsFile).recreate(config.inputFile, config.outputFile)
            }
          }
          if (config.postprocess) {
            val parametersMap = Map(
                "processed_pdb" -> config.outputFile.toString,
                "prepared_mae" -> "prepared.mae",
                "input_csb" -> "sb_config.csb",
                "prepared_cms" -> "prepared.cms",
                "processed_cms" -> "processed.cms",
                "desmond_config" -> "desmond_config.cfg",
                "desmond_total_time" -> "%8.3f".format(20.0),
                "desmond_time_interval" -> "%8.3f".format(1.0),
                "desmond_output_cms" -> "md_result.cms")
          //next goes postprocessing
            prepareCSB(parametersMap)
            prepareCFG(parametersMap)
            val postpocessingCommandsSource = Source.fromURL(getClass.getResource("/postprocessing.txt"))
            try {
              (new ScriptRunner(postpocessingCommandsSource)).run(parametersMap)
            }
            finally {
              postpocessingCommandsSource.close()
            }
          }

        } catch {
          case e : Exception =>
            //logger.error(s"Fatal error: ${e.getMessage}")
            e.printStackTrace()
            if (config.debug) {
              //e.printStackTrace()
            }
        }
      case None => parser.showUsage
    }
  }
  def prepareCSB(hash : Map[String, String]) = hash.get("input_csb") match {
    case Some(configFileName) => {
      val preprocessorTemplateSource = Source.fromURL(getClass.getResource("/sb_config.csb.template"))
      val preprocessorLines = new ScriptPreprocessor(preprocessorTemplateSource).substitute(hash)

      val pw = new PrintWriter(configFileName)
      preprocessorLines.foreach({ l => pw.write(l); pw.write("\n")  })
      pw.close()
      preprocessorTemplateSource.close()
    }
    case None => ()
  }

  def prepareCFG(hash : Map[String, String]) = hash.get("desmond_config") match {
    case Some(configFileName) => {
      val preprocessorTemplateSource = Source.fromURL(getClass.getResource("/desmond.cfg.template"))
      val preprocessorLines = new ScriptPreprocessor(preprocessorTemplateSource).substitute(hash, false)

      val pw = new PrintWriter(configFileName)
      preprocessorLines.foreach({
        l => pw.write(l); pw.write("\n")
      })
      pw.close()
      preprocessorTemplateSource.close()
    }
    case None => ()
  }
}
