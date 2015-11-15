package ru.biocad.ig.common.io.pdb


//TODO: should extend all logic of pdb file reading, the following class is temporary solution for collecting atoms to aminoacids
case class PDBAminoacidCollection(sequence : Seq[PDBAtomInfo]) {
  val aminoacidsByChain = new PDBChainIterator(sequence)
  val aminoacids = sequence.groupBy(_.chainID).map(x=>(x._1, x._2.groupBy(_.resSeq))).toMap
  val aminoacidIds = sequence.groupBy(_.chainID).map(x=>(x._1, x._2.map(_.resSeq).distinct)).toMap

  val chains = aminoacids.keys
}

object PDBAminoacidCollection {
  def apply(basic_structure : PDBStructure) = new PDBAminoacidCollection(basic_structure.parse_array.toSeq)
}
