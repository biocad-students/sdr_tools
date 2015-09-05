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

import ru.biocad.ig.common.algorithms.geometry.AminoacidUtils


object SimplifiedAACreationTest {
  def main(args : Array[String]) = {
    println("testing")
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
import EOneJsonProtocol._
import ru.biocad.ig.common.structures.geometry.Lattice


object energyTermsJSONLoadingTest{
  def loadStructure(filename : String) = {
    println("loading structure from sample pdb...")
    val structure : PDBStructure = new PDBStructure()
    structure.readFile(getClass.getResource(filename).getFile())
    println("local file read - done")
    val aa_by_chain = new PDBAminoAcidCollection(structure)
    val aas = aa_by_chain.aminoacidIds('L')
    println(aas)
    val filtered_map = aas.map(aa => new SimplifiedAminoAcid(aa_by_chain.aminoacids('L')(aa)))
    println(filtered_map.head.toString)
    filtered_map
    println(Lattice.getEnergy(filtered_map))
  }
  def main(args : Array[String]) = {
    println("testing JSON")
    //val bi = JsonParser(Source.fromURL(getClass.getResource("/backbone.json")).getLines().mkString("")).convertTo[Map[String, Map[String, Map[String, Seq[Double]]]]]
    //println(bi("LEU")("(20,22,-32)"))
    val eone : EOne = JsonParser(Source.fromURL(getClass.getResource("/MCDP_json/EONE.json")).getLines().mkString("")).convertTo[EOne]
    println("testing backbone reconstruction...")
    val simplifiedChain = loadStructure("/2OSL.pdb")



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
      val coordinatesMap = backboneInfo.restoreInfo(a2.name, d1, d2, d3)
      val (x, y, z) = AminoacidUtils.getLocalCoordinateSystem(a1.ca, a2.ca, a3.ca, a4.ca)
      coordinatesMap.data.map({
        case (k, v) => (k, AminoacidUtils.getGlobalCoordinates(Seq(x, y, z), v.toSeq))
      })
    }}).toList
    println(result)
  }
}
