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

  private case class Config(inputFile : File = new File("2OSL.pdb"),
      outputFile : File = new File("result.pdb"),
      numberOfMoves : Int = 100,
      //TODO: add option - refine/fold/alascan - default refine
      debug : Boolean = false)

  private def getParser = new scopt.OptionParser[Config]("sdr_tools") {
      opt[File]('i', "inputFile") action {(s, c) => c.copy(inputFile = s)} text "input file in PDB format"
      opt[File]('o', "outputFile") action {(s, c) => c.copy(outputFile = s)} text "output file in PDB format"
      opt[Int]('n', "numberOfMoves") action {(s, c) => c.copy(numberOfMoves = s)} text "number of iterations (moves) to call MC"
      opt[Unit]("debug") action {(_, c) => c.copy(debug = true)} text "enable debug output"
      help("help") text "this message"
  }

  def main(args : Array[String]) : Unit = {
    val parser = getParser
    parser.parse(args, Config()) match {
      case Some(config) =>
        try {
          MonteCarloRunner.run(config.inputFile, config.outputFile, config.numberOfMoves)
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
