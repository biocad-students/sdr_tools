package test.alphabet

import ru.biocad.ig.common.io.pdb.{PDBStructure, PDBAtomInfo, PDBAminoAcidCollection}

import ru.biocad.ig.common.structures.geometry._

import ru.biocad.ig.common.structures.aminoacid.{SimplifiedAminoacid}
import spray.json._
//import DefaultJsonProtocol._
import scala.io.Source

import ru.biocad.ig.alascan.constants.json.BackboneLibraryJsonProtocol
import ru.biocad.ig.alascan.constants.json.BackboneLibraryJsonProtocol._
import ru.biocad.ig.alascan.constants.{AminoacidLibrary, BackboneInfo}

import ru.biocad.ig.alascan.constants.json.BasicVectorLibraryJsonProtocol._
import ru.biocad.ig.alascan.constants.json.BasicVectorLibrary

import ru.biocad.ig.common.algorithms.geometry.AminoacidUtils


import ru.biocad.ig.alascan.constants.energy_terms._

import ru.biocad.ig.common.algorithms._

object MCTest{
  def loadStructure(filename : String) = {
    println("loading structure from sample pdb...")
    val structure : PDBStructure = new PDBStructure()
    structure.readFile(getClass.getResource(filename).getFile())
    println("local file read - done")
    val aa_by_chain = new PDBAminoAcidCollection(structure)
    val aas = aa_by_chain.aminoacidIds('L')
    val filtered_map = aas.map(aa => SimplifiedAminoacid(aa_by_chain.aminoacids('L')(aa))).toArray
    //println(filtered_map.head.toString)
    (filtered_map, aas.map(aa => aa_by_chain.aminoacids('L')(aa)).toArray)
  }

  def main(args : Array[String]) : Unit = {
    println("testing backbone reconstruction...")
    val (simplifiedChain, fullAtomChain) = loadStructure("/2OSL.pdb")
    println(Lattice.getEnergy(simplifiedChain))
    val backboneVectors : Array[GeometryVector] = JsonParser(Source.fromURL(getClass.getResource("/basic_vectors.json")).getLines()
      .mkString("")).convertTo[BasicVectorLibrary].vectors.map({
          x : Seq[Double] => new Vector(x)
        }).toArray

    //println(Lattice.getEnergy(simplifiedChain))
    val ch1 = MonteCarloRunner.run(simplifiedChain,
      Seq(new BondMove(backboneVectors, 3),
        new RotamerMove(Lattice.sidechainsInfo)),
        x => Lattice.getEnergy(x.toArray), 10)
    println(Lattice.getEnergy(ch1.toArray))
    Lattice.toFullAtomRepresentation(ch1, fullAtomChain)
    //val sidechainInfo = JsonParser(Source.fromURL(getClass.getResource("/sidechains.json")).getLines().mkString("")).convertTo[AminoacidLibrary[SidechainInfo]]
  }
}
