package test.alphabet

//import DefaultJsonProtocol._
import scala.io.Source
import scopt.OptionParser
import java.io.File
import com.typesafe.scalalogging.slf4j.LazyLogging

import ru.biocad.ig.common.algorithms.MonteCarloRunner


/** Selects method of data processing from command line parameters
  * should have several ways to invoke:
  * 1. input file in pdb format with structure to refine/fold
  * 2. input file in pdb to perform alanine scanning
  * 3. input file in fasta or as a sequence of aminoacids - to compare with known folding
  */
object MCTest extends LazyLogging {

  private case class Config(input_pdb_file : File = new File("2OSL.pdb"),
      output_pdb_file : File = null,
      number_of_moves : Int = 100,
      //TODO: add option - refine/fold/alascan - default refine
      debug : Boolean = false)

  private def getParser = new scopt.OptionParser[Config]("sdr_tools") {
      opt[File]('i', "input_pdb_file") action {(s, c) => c.copy(input_pdb_file = s)} text "input file in PDB format"
      opt[File]('o', "output_pdb_file") action {(s, c) => c.copy(output_pdb_file = s)} text "output file in PDB format"
      opt[Int]('n', "number_of_moves") action {(s, c) => c.copy(number_of_moves = s)} text "number of iterations (moves) to call MC"
      opt[Unit]("debug") action {(_, c) => c.copy(debug = true)} text "enable debug output"
      help("help") text "this message"
  }

  def main(args : Array[String]) : Unit = {
    val parser = getParser
    parser.parse(args, Config()) match {
      case Some(config) =>
        try {
          MonteCarloRunner.run(config.input_pdb_file, config.output_pdb_file, config.number_of_moves)
        } catch {
          case e : Exception =>
            logger.error(s"Fatal error: ${e.getMessage}")
            if (config.debug) {
              e.printStackTrace()
            }
        }
      case None => parser.showUsage
    }

  }
}
