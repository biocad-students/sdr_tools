package ru.biocad.ig.common.algorithms


import java.io.{File}
import scala.io.Source
import com.typesafe.scalalogging.slf4j.LazyLogging

import ru.biocad.ig.common.io.pdb.{PDBStructure, PDBAtomInfo, PDBAminoacidCollection}

import ru.biocad.ig.common.structures.geometry._
import ru.biocad.ig.alascan.moves._

import ru.biocad.ig.common.structures.aminoacid.SimplifiedChain

import ru.biocad.ig.common.io.pdb.PDBWriter


/** Selects method of data processing from command line parameters.
  * Calls actual MC algorithm, which defined at [[ru.biocad.ig.common.algorithms.MonteCarlo]] object, with different parameters
  */
case class MonteCarloRunner(val lattice : Lattice) extends LazyLogging {

  /** helper method, loads structure from Protein Data Bank file and then returns simplified representation to it.
    * NB! - Currently it supposes that PDB file contains valid structure, where all atoms are well-positioned
    * returns data for one particular chain, because current MC implementation supports 1-chained proteins.
    *
    * @param file - valid pdb file to process (all checks are neglected, because I'm a simple codemonkey with no hope, faith, fear, and future)
    * @param chain chain letter from corresponding ATOM portion from PDB data (chain names can also be found in structure description somewhere at the beginning)
    * @return pair of 2 objects - MC-ready simplified structure and sequence of pdb 'ATOM' lines, corresponding to chain params, grouped for each chain's aminoacid
    */
  def loadStructure(file : File, chain : Char) : (SimplifiedChain, Seq[Seq[PDBAtomInfo]]) = {
    println("loading structure from sample pdb...")
    val structure : PDBStructure = new PDBStructure()
    val fileSource = Source.fromFile(file)
    try
      structure.readFile(fileSource)
    finally fileSource.close()

    println("local file read - done")
    val aaByChain = PDBAminoacidCollection(structure)
    val aas = aaByChain.aminoacidsByChain.toSeq
    val filteredMap = SimplifiedChain(aas, lattice)//TODO : init lattice somewhere near this
    (filteredMap, aas)
  }

  def loadStructure(filename : String, chain : Char = 'L') : (SimplifiedChain, Seq[Seq[PDBAtomInfo]])  = loadStructure(new File(filename), chain)


  /** improves current PDB structure (one chain gets improved, multichains are not supported now)
    *
    * @param inputFile PDB file with protein to improve
    * @param mcTimeUnits MC parameter, actual number of timeUnits
    */
  def refine(inputFile : File, mcTimeUnits : Int, outputFile : File, chain : Char = 'L') = {
    println("start of refinement...")
    val (simplifiedChain, fullAtomChain) = loadStructure(inputFile, chain)
    logger.info("Energy before structure refinement: " + lattice.getEnergy(simplifiedChain).toString)

    //println(Lattice.getEnergy(simplifiedChain))
    val ch1 = MonteCarlo(lattice).run(simplifiedChain,
        getMovesForSequence(simplifiedChain.size),
        x => lattice.getEnergy(x), mcTimeUnits)
    logger.info("Energy after structure refinement: "+ lattice.getEnergy(ch1))
    val result = lattice.toFullAtomRepresentation(ch1, fullAtomChain)
    //val sidechainInfo = JsonParser(Source.fromURL(getClass.getResource("/sidechains.json")).getLines().mkString("")).convertTo[AminoacidLibrary[SidechainInfo]]
    val w = new PDBWriter(outputFile)
    try
      w.writeAtomInfo(result)
    finally
      w.close()
  }

  def getMovesForSequence(n : Int) = {
     Seq(
      (new BondMove(lattice.backboneVectors, lattice.sidechainsInfo, 2, true, Some(lattice.latticeConstants)), n - 2),
      (new BondMove(lattice.backboneVectors, lattice.sidechainsInfo, 4), n - 4),
      (new BondMove(lattice.backboneVectors, lattice.sidechainsInfo, 6), n - 6),
      (new BondMove(lattice.backboneVectors, lattice.sidechainsInfo, 8), n - 8),
      (new BondMove(lattice.backboneVectors, lattice.sidechainsInfo, 10), n - 10),
      (new DisplacementMove(lattice.backboneVectors, lattice.sidechainsInfo), 2),
      (new RotamerMove(lattice.sidechainsInfo), n)
    ).filter(_._2 > 0)
  }

  /** computes folded structure, starts from given aminoacid sequence
    *
    * @param sequence
    * @param mcTimeUnits
    * @param outputFile
    */
  def fold(sequence : String, mcTimeUnits : Int, outputFile : File) = {
    println("start chain folding...")
    val simplifiedChain = SimplifiedChain.fromSequence(sequence, lattice)
    logger.info("Energy before structure fold: " + lattice.getEnergy(simplifiedChain).toString)

    //println(Lattice.getEnergy(simplifiedChain))
    val ch1 = MonteCarlo(lattice).run(simplifiedChain, getMovesForSequence(simplifiedChain.size),
        x => lattice.getEnergy(x), mcTimeUnits)
    logger.info("Energy after structure fold: "+ lattice.getEnergy(ch1))
    val result = lattice.toFullAtomRepresentation(ch1)
    val w = new PDBWriter(outputFile)
    try
      w.writeAtomInfo(result)
    finally
      w.close()
  }

  //TODO: change to alascan
  /** Should perform alanine scanning - currently NOT tested, currently copies refine method
    * Idea : in cycle change 1 aminoacid in sequence and perform 1 mc run. quite easy.
    * The main problem is to made genuine, informative report file.
    */
  def scan(inputFile : File, mcTimeUnits : Int, outputFile : File, chain : Char = 'L') = {
    println("start scan...")
    val (simplifiedChain, fullAtomChain) = loadStructure(inputFile, chain)
    logger.info("Energy before structure alascan: " + lattice.getEnergy(simplifiedChain).toString)

    //println(Lattice.getEnergy(simplifiedChain))
    val ch1 = MonteCarlo(lattice).run(simplifiedChain, getMovesForSequence(simplifiedChain.size),
        x => lattice.getEnergy(x), mcTimeUnits)
    logger.info("Energy after structure alascan: "+ lattice.getEnergy(ch1))
    val result = lattice.toFullAtomRepresentation(ch1, fullAtomChain)
    //val sidechainInfo = JsonParser(Source.fromURL(getClass.getResource("/sidechains.json")).getLines().mkString("")).convertTo[AminoacidLibrary[SidechainInfo]]
    val w = new PDBWriter(outputFile)
    try
      w.writeAtomInfo(result)
    finally
      w.close()
  }

  /** Converts structure from input file to simplified representation, when loads it to output file.
    * This method is used to compare different implementations of full-chain reconstruction methods
    * @param inputFile pdb file to load structure from
    * @param outputFile pdb file to write structure to
    */
  def recreate(inputFile : File, outputFile : File, chain : Char = 'L') = {
    println("load simplifiedChain")
    val (simplifiedChain, fullAtomChain) = loadStructure(inputFile, chain)
    logger.info("Input file energy: " + lattice.getEnergy(simplifiedChain).toString)
    val result = lattice.toFullAtomRepresentation(simplifiedChain, fullAtomChain)
    val s2 = SimplifiedChain((new PDBAminoacidCollection(result)).aminoacidsByChain.toSeq, lattice)
    logger.info("Energy after recreate: " + lattice.getEnergy(s2).toString)
    val w = new PDBWriter(outputFile)
    try
      w.writeAtomInfo(result)
    finally
      w.close()
  }

}

object MonteCarloRunner {
  def apply(settingsFile : File) = new MonteCarloRunner(new Lattice(settingsFile))
}
