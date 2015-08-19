package test.alphabet

import ru.biocad.ig.common.io.pdb.{PDBStructure, PDBAtomInfo, PDBAminoAcidCollection}

import ru.biocad.ig.common.structures.geometry.{
    Vector, Vector2d, Vector3d,InfiniteVector,
    SimplifiedAminoAcid
  }
import spray.json._
import DefaultJsonProtocol._
import scala.io.Source
//import ru.biocad.ig.alascan.constants.json.AlascanConstantsJsonProtocol
//import ru.biocad.ig.alascan.constants.json.AlascanConstantsJsonProtocol._
import ru.biocad.ig.alascan.constants.BackboneInfo
//TODO: update scala, find out wtf wrong with alphabet's calling


object SimplifiedAACreationTest{
  def main(args : Array[String]) = {
    println("testing")
    val structure : PDBStructure = new PDBStructure()
    structure.readFile("2OSL.pdb")
    //println(typename(aa_by_chain.aminoacids) )
    val aa_by_chain = new PDBAminoAcidCollection(structure)
    val aas = aa_by_chain.aminoacids('L').keys.toSeq.sorted.take(5)
    println(aas)
    val filtered_map = aas.map(aa => new SimplifiedAminoAcid(aa_by_chain.aminoacids('L')(aa)))
    println(filtered_map.head.toString)
    println(Vector3d(1, 0, 0)**Vector3d(0,1,0))
  }
}

object JSONLoadingTest{
  def main(args : Array[String]) = {
    println("testing JSON")
    val bi = JsonParser(Source.fromFile("backbone.json").getLines().mkString("")).convertTo[Map[String, Map[String, Map[String, Seq[Double]]]]]
    println(bi("LEU")("(20,22,-32)"))
  }
}

object SubchainBackboneReconstructionTest{
  def main(args : Array[String]) = {
    println("testing backbone reconstruction...")
    val structure : PDBStructure = new PDBStructure()
    structure.readFile("2OSL.pdb")
    println("local file read - ok")
    val aa_by_chain = new PDBAminoAcidCollection(structure)
    val aas = aa_by_chain.aminoacids('L').keys.toSeq.sorted.take(5)
    println(aas)
    val filtered_map = aas.map(aa => new SimplifiedAminoAcid(aa_by_chain.aminoacids('L')(aa)))
    println(filtered_map.head.atomsMap.toString)
  }
}
