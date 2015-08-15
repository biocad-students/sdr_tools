package test.alphabet

import ru.biocad.ig.common.io.pdb.{PDBStructure, PDBAtomInfo, PDBAminoAcidCollection}

import ru.biocad.ig.common.structures.geometry.{
    Vector, Vector2d, Vector3d,InfiniteVector,
    SimplifiedAminoAcid
  }

//TODO: update scala, find out wtf wrong with alphabet's calling


object SimplifiedAATest{

  def main(args : Array[String]) = {
    println("testing")
    val structure : PDBStructure = new PDBStructure()
    structure.readFile("2OSL.pdb")
    val aa_by_chain = new PDBAminoAcidCollection(structure)
    //println(typename(aa_by_chain.aminoacids) )
    println(aa_by_chain.chains)
    val aas = aa_by_chain.aminoacids('L').keys.take(5).toSeq
    println(aas)
    val filtered_map = aa_by_chain.aminoacids('L').filter(k=>aas.contains(k._1)).map( a =>
    {
      new SimplifiedAminoAcid(a._2)
    })
    println(filtered_map.head.toString)

  }
}
