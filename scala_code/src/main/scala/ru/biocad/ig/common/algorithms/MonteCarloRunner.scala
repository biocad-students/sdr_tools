package ru.biocad.ig.common.algorithms


import java.io.File
import com.typesafe.scalalogging.slf4j.LazyLogging

import ru.biocad.ig.common.io.pdb.{PDBStructure, PDBAtomInfo, PDBAminoacidCollection}

import ru.biocad.ig.common.structures.geometry._
import ru.biocad.ig.alascan.moves._

import ru.biocad.ig.common.structures.aminoacid.SimplifiedChain

import ru.biocad.ig.common.io.pdb.PDBWriter


/** Selects method of data processing from command line parameters.
  * Calls actual MC algorithm, which defined at [[ru.biocad.ig.common.algorithms.MonteCarlo]] object, with different parameters
  */
object MonteCarloRunner extends LazyLogging {

  /** helper method, loads structure from Protein Data Bank file and then returns simplified representation to it.
    * NB! - Currently it supposes that PDB file contains valid structure, where all atoms are well-positioned
    * returns data for one particular chain, because current MC implementation supports 1-chained proteins.
    *
    * @param filename - name of valid pdb filename to process (all checks are neglected, because I'm a simple codemonkey with no hope, faith, fear, and future)
    * @param chain chain letter from corresponding ATOM portion from PDB data (chain names can also be found in structure description somewhere at the beginning)
    * @return pair of 2 objects - MC-ready simplified structure and sequence of pdb 'ATOM' lines, corresponding to chain params, grouped for each chain's aminoacid
    */
  def loadStructure(filename : String, chain : Char = 'A')  : (SimplifiedChain, Seq[Seq[PDBAtomInfo]]) = {
    println("loading structure from sample pdb...")
    val structure : PDBStructure = new PDBStructure()
    structure.readFile(getClass.getResource(filename).getFile())
    println("local file read - done")
    val aaByChain = PDBAminoacidCollection(structure)
    val aas = aaByChain.aminoacidsByChain.toSeq
    val filteredMap = SimplifiedChain(aas)
    (filteredMap, aas)
  }

  /** improves current PDB structure (one chain gets improved, multichains are not supported now)
    *
    * @param inputFile PDB file with protein to improve
    * @param mcTimeUnits MC parameter, actual number of timeUnits
    */
  def refine(inputFile : File, mcTimeUnits : Int, outputFile : File) = {
    println("testing backbone reconstruction...")
    val (simplifiedChain, fullAtomChain) = loadStructure("/2OSL.pdb")
    logger.info("Energy before structure refinement: " + Lattice.getEnergy(simplifiedChain).toString)

    //println(Lattice.getEnergy(simplifiedChain))
    val ch1 = MonteCarlo.run(simplifiedChain,
        getMovesForSequence(simplifiedChain.size),
        x => Lattice.getEnergy(x), mcTimeUnits)
    logger.info("Energy after structure refinement: "+ Lattice.getEnergy(ch1))
    val result = Lattice.toFullAtomRepresentation(ch1, fullAtomChain)
    //val sidechainInfo = JsonParser(Source.fromURL(getClass.getResource("/sidechains.json")).getLines().mkString("")).convertTo[AminoacidLibrary[SidechainInfo]]
    val w = new PDBWriter(outputFile)
    w.writeAtomInfo(result)
    w.close()
  }

  def recreate(inputFile : File, outputFile : File) = {
    println("load simplifiedChain")
    val (simplifiedChain, fullAtomChain) = loadStructure("/result_old.pdb")
    logger.info("Input file energy: " + Lattice.getEnergy(simplifiedChain).toString)
    val result = Lattice.toFullAtomRepresentation(simplifiedChain, fullAtomChain)
    val s2 = SimplifiedChain((new PDBAminoacidCollection(result)).aminoacidsByChain.toSeq)
    logger.info("Energy after recreate: " + Lattice.getEnergy(s2).toString)
    val w = new PDBWriter(outputFile)
    w.writeAtomInfo(result)
    w.close()
  }

  def getMovesForSequence(n : Int) = {
     Seq(
      (new BondMove(Lattice.backboneVectors, 2), n - 2),
      (new BondMove(Lattice.backboneVectors, 4), n - 4),
      (new BondMove(Lattice.backboneVectors, 6), n - 6),
      (new BondMove(Lattice.backboneVectors, 8), n - 8),
      (new BondMove(Lattice.backboneVectors, 10), n - 10),
      (new DisplacementMove(Lattice.backboneVectors), 2),
      (new RotamerMove(Lattice.sidechainsInfo), n)
    ).filter(_._2 > 0)
  }

  /** computes folded structure, starts from given aminoacid sequence
    *
    * @param sequence
    * @param mcTimeUnits
    * @param outputFile
    */
  def fold(sequence : String, mcTimeUnits : Int, outputFile : File) = {
    println("testing backbone reconstruction...")
    val simplifiedChain = SimplifiedChain.fromSequence(sequence, Lattice.sidechainsInfo)
    logger.info("Energy before structure fold: " + Lattice.getEnergy(simplifiedChain).toString)

    //println(Lattice.getEnergy(simplifiedChain))
    val ch1 = MonteCarlo.run(simplifiedChain, getMovesForSequence(simplifiedChain.size),
        x => Lattice.getEnergy(x), mcTimeUnits)
    logger.info("Energy after structure fold: "+ Lattice.getEnergy(ch1))
    val result = Lattice.toFullAtomRepresentation(ch1)
    val w = new PDBWriter(outputFile)
    w.writeAtomInfo(result)
    w.close()
    //TODO: construct full-atom chain with no pdb atom details
  }

  //TODO: change to alascan
  /** Should perform alanine scanning - currently NOT tested, currently copies refine method
    * Idea : in cycle change 1 aminoacid in sequence and perform 1 mc run. quite easy.
    * The main problem is to made genuine, informative report file.
    */
  def scan(inputFile : File, mcTimeUnits : Int, outputFile : File) = {
    ???
    println("testing backbone reconstruction...")
    val (simplifiedChain, fullAtomChain) = loadStructure("/2OSL.pdb")
    logger.info("Energy before structure scan: " + Lattice.getEnergy(simplifiedChain).toString)

    //println(Lattice.getEnergy(simplifiedChain))
    val ch1 = MonteCarlo.run(simplifiedChain, getMovesForSequence(simplifiedChain.size),
        x => Lattice.getEnergy(x), mcTimeUnits)
    logger.info("Energy after structure scan: "+ Lattice.getEnergy(ch1))
    val result = Lattice.toFullAtomRepresentation(ch1, fullAtomChain)
    //val sidechainInfo = JsonParser(Source.fromURL(getClass.getResource("/sidechains.json")).getLines().mkString("")).convertTo[AminoacidLibrary[SidechainInfo]]
    val w = new PDBWriter(outputFile)
    w.writeAtomInfo(result)
    w.close()
  }

}
