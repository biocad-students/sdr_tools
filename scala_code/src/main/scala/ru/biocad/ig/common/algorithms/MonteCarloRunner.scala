package ru.biocad.ig.common.algorithms


import spray.json._
import scala.io.Source
import java.io.File
import com.typesafe.scalalogging.slf4j.LazyLogging

import ru.biocad.ig.common.io.pdb.{PDBStructure, PDBAtomInfo, PDBAminoAcidCollection}

import ru.biocad.ig.common.structures.geometry._

import ru.biocad.ig.common.structures.aminoacid.{SimplifiedAminoacid}


import ru.biocad.ig.alascan.constants.json.BackboneLibraryJsonProtocol
import ru.biocad.ig.alascan.constants.json.BackboneLibraryJsonProtocol._
import ru.biocad.ig.alascan.constants.{AminoacidLibrary, BackboneInfo}

import ru.biocad.ig.alascan.constants.json.BasicVectorLibraryJsonProtocol._
import ru.biocad.ig.alascan.constants.json.BasicVectorLibrary

import ru.biocad.ig.common.algorithms.geometry.AminoacidUtils


import ru.biocad.ig.alascan.constants.energy_terms._



/** Selects method of data processing from command line parameters
  */
object MonteCarloRunner extends LazyLogging {

  def loadStructure(filename : String, chain : Char = 'L') = {
    println("loading structure from sample pdb...")
    val structure : PDBStructure = new PDBStructure()
    structure.readFile(getClass.getResource(filename).getFile())
    println("local file read - done")
    val aa_by_chain = new PDBAminoAcidCollection(structure)
    val aas = aa_by_chain.aminoacidIds(chain)
    val filtered_map = aas.map(aa => SimplifiedAminoacid(aa_by_chain.aminoacids(chain)(aa))).toArray
    //println(filtered_map.head.toString)
    (filtered_map, aas.map(aa => aa_by_chain.aminoacids(chain)(aa)).toArray)
  }

  def run(input_pdb_file : File, output_pdb_file : File, number_of_moves : Int) = {
    println("testing backbone reconstruction...")
    val (simplifiedChain, fullAtomChain) = loadStructure("/2OSL.pdb")
    logger.info("Energy before structure refinement: " + Lattice.getEnergy(simplifiedChain).toString)

    //println(Lattice.getEnergy(simplifiedChain))
    val ch1 = MonteCarlo.run(simplifiedChain,
      Seq(new BondMove(Lattice.backboneVectors, 3),
        new RotamerMove(Lattice.sidechainsInfo)),
        x => Lattice.getEnergy(x.toArray), number_of_moves)
    logger.info("Energy after structure refinement: "+ Lattice.getEnergy(ch1.toArray))
    Lattice.toFullAtomRepresentation(ch1, fullAtomChain)
    //val sidechainInfo = JsonParser(Source.fromURL(getClass.getResource("/sidechains.json")).getLines().mkString("")).convertTo[AminoacidLibrary[SidechainInfo]]


  }

}
