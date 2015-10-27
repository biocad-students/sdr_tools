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

import ru.biocad.ig.common.io.pdb.PDBWriter


/** Selects method of data processing from command line parameters
  */
object MonteCarloRunner extends LazyLogging {

  def loadStructure(filename : String, chain : Char = 'L') = {
    println("loading structure from sample pdb...")
    val structure : PDBStructure = new PDBStructure()
    structure.readFile(getClass.getResource(filename).getFile())
    println("local file read - done")
    val aaByChain = new PDBAminoAcidCollection(structure)
    val aas = aaByChain.aminoacidIds(chain)
    val filteredMap = aas.map(aa => SimplifiedAminoacid(aaByChain.aminoacids(chain)(aa))).toArray
    (filteredMap, aas.map(aa => aaByChain.aminoacids(chain)(aa)).toArray)
  }

  def run(inputFile : File, outputFile : File, numberOfMoves : Int) = {
    println("testing backbone reconstruction...")
    val (simplifiedChain, fullAtomChain) = loadStructure("/2OSL.pdb")
    logger.info("Energy before structure refinement: " + Lattice.getEnergy(simplifiedChain).toString)

    //println(Lattice.getEnergy(simplifiedChain))
    val ch1 = MonteCarlo.run(simplifiedChain,
      Seq(new BondMove(Lattice.backboneVectors, 3),
        new RotamerMove(Lattice.sidechainsInfo)),
        x => Lattice.getEnergy(x.toArray), numberOfMoves)
    logger.info("Energy after structure refinement: "+ Lattice.getEnergy(ch1.toArray))
    val result = Lattice.toFullAtomRepresentation(ch1, fullAtomChain)
    //val sidechainInfo = JsonParser(Source.fromURL(getClass.getResource("/sidechains.json")).getLines().mkString("")).convertTo[AminoacidLibrary[SidechainInfo]]
    val w = new PDBWriter(outputFile)
    w.writeAtomInfo(result)
    w.close()

  }

}
