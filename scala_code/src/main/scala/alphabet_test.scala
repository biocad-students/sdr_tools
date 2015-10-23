package test.alphabet

import ru.biocad.ig.common.io.pdb.{PDBStructure, PDBAtomInfo, PDBAminoAcidCollection}

import ru.biocad.ig.common.structures.geometry.{
    Vector, Vector2d, Vector3d,InfiniteVector}

import ru.biocad.ig.common.structures.aminoacid.{SimplifiedAminoAcid}
import spray.json._
//import DefaultJsonProtocol._
import scala.io.Source

import ru.biocad.ig.alascan.constants.json.BackboneLibraryJsonProtocol
import ru.biocad.ig.alascan.constants.json.BackboneLibraryJsonProtocol._
import ru.biocad.ig.alascan.constants.{AminoacidLibrary, BackboneInfo}
//TODO: update scala, find out wtf wrong with alphabet's calling

import ru.biocad.ig.alascan.constants.json.BasicVectorLibraryJsonProtocol._
import ru.biocad.ig.alascan.constants.json.BasicVectorLibrary

import ru.biocad.ig.common.algorithms.geometry.AminoacidUtils
import ru.biocad.ig.common.structures.geometry.GeometryVector

object SimplifiedAACreationTest {
  def main(args : Array[String]) = {
    println("testing")
    val backboneVectors : Seq[GeometryVector] = JsonParser(Source.fromURL(getClass.getResource("/basic_vectors.json")).getLines()
      .mkString("")).convertTo[BasicVectorLibrary].vectors.map({x:Seq[Double] => new Vector(x)})
    println(backboneVectors(0))

    val structure : PDBStructure = new PDBStructure()
    structure.readFile(getClass.getResource("/2OSL.pdb").getFile())//"2OSL.pdb")
    //println(typename(aa_by_chain.aminoacids) )
    val aa_by_chain = new PDBAminoAcidCollection(structure)
    val aas = aa_by_chain.aminoacids('L').keys.toSeq.sorted.take(5)
    println(aas)
    val filtered_map = aas.map(aa => new SimplifiedAminoAcid(aa_by_chain.aminoacids('L')(aa)))
    println(filtered_map.head.toString)
  }
}

import ru.biocad.ig.alascan.constants.energy_terms._

import ru.biocad.ig.common.structures.geometry.Lattice
import ru.biocad.ig.common.structures.geometry._

import ru.biocad.ig.common.algorithms._
object MCTest{
  def loadStructure(filename : String) = {
    println("loading structure from sample pdb...")
    val structure : PDBStructure = new PDBStructure()
    structure.readFile(getClass.getResource(filename).getFile())
    println("local file read - done")
    val aa_by_chain = new PDBAminoAcidCollection(structure)
    val aas = aa_by_chain.aminoacidIds('L')
    val filtered_map = aas.map(aa => new SimplifiedAminoAcid(aa_by_chain.aminoacids('L')(aa))).toArray
    println(filtered_map.head.toString)
    filtered_map
  }

  def main(args : Array[String]) : Unit = {
    println("testing backbone reconstruction...")
    val simplifiedChain = loadStructure("/2OSL.pdb")
    println(Lattice.getEnergy(simplifiedChain))
    val backboneVectors : Array[GeometryVector] = JsonParser(Source.fromURL(getClass.getResource("/basic_vectors.json")).getLines()
      .mkString("")).convertTo[BasicVectorLibrary].vectors.map({x:Seq[Double] => new Vector(x)}).toArray

    //println(Lattice.getEnergy(simplifiedChain))
    val ch1 = MonteCarloRunner.run(simplifiedChain,
      Seq(new BondMove(backboneVectors, 3)),
      x=>Lattice.getEnergy(x.toArray),
      10)
    println(Lattice.getEnergy(ch1.toArray))
    //ch1.map(Lattice.toFullAtomRepresentation)
    //val sidechainInfo = JsonParser(Source.fromURL(getClass.getResource("/sidechains.json")).getLines().mkString("")).convertTo[AminoacidLibrary[SidechainInfo]]



  }

}

object energyTermsJSONLoadingTest{
  def loadStructure(filename : String) = {
    println("loading structure from sample pdb...")
    val structure : PDBStructure = new PDBStructure()
    structure.readFile(getClass.getResource(filename).getFile())
    println("local file read - done")
    val aa_by_chain = new PDBAminoAcidCollection(structure)
    val aas = aa_by_chain.aminoacidIds('L')
    println(aas)
    val filtered_map = aas.map(aa => new SimplifiedAminoAcid(aa_by_chain.aminoacids('L')(aa))).toArray
    println(filtered_map.head.toString)
    filtered_map
  }

  def main(args : Array[String]) = {
    println("testing JSON")
    //val bi = JsonParser(Source.fromURL(getClass.getResource("/backbone.json")).getLines().mkString("")).convertTo[Map[String, Map[String, Map[String, Seq[Double]]]]]
    //println(bi("LEU")("(20,22,-32)"))

    println("testing backbone reconstruction...")
    val simplifiedChain = loadStructure("/2OSL.pdb")
    println(Lattice.getEnergy(simplifiedChain))
  }
}


object SubchainBackboneReconstructionTest{
  def main(args : Array[String]) : Unit = {
    println("testing backbone reconstruction...")
    val structure : PDBStructure = new PDBStructure()
    structure.readFile(getClass.getResource("/2OSL.pdb").getFile())
    println("local file read - ok")
    val aa_by_chain = new PDBAminoAcidCollection(structure)
    val aas = aa_by_chain.aminoacids('L').keys.toSeq.sorted.take(5)
    println(aas)
    val filtered_map = aas.map(aa => new SimplifiedAminoAcid(aa_by_chain.aminoacids('L')(aa)))
    println(filtered_map.head.atomsMap.toString)
    val backboneInfo = JsonParser(Source.fromURL(getClass.getResource("/backbone.json")).getLines().mkString("")).convertTo[AminoacidLibrary[BackboneInfo]]
    val result = filtered_map.sliding(4, 1).map({case Seq(a1, a2, a3, a4) => {
      val (d1, d2, d3) = AminoacidUtils.getDistances(a1.ca, a2.ca, a3.ca, a4.ca)
      val coordinatesMap = backboneInfo.restoreAminoAcidInfo(a2.name, d1, d2, d3)
      val (x, y, z) = AminoacidUtils.getLocalCoordinateSystem(a1.ca, a2.ca, a3.ca, a4.ca)
      coordinatesMap.data.map({
        case (k, v) => (k, AminoacidUtils.getGlobalCoordinates(Seq(x, y, z), v.toSeq))
      })
    }}).toList
    println(result)
  }
}
