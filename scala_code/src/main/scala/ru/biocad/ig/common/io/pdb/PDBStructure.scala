package ru.biocad.ig.common.io.pdb

import scala.collection.mutable.ArrayBuffer

//helper class, contains all information about atomic structure for given PDB
class PDBStructure {
  val parse_array : ArrayBuffer[PDBAtomInfo] = ArrayBuffer.empty[PDBAtomInfo]

  def readFile(filename : String) : Unit = {
    var pdbReader: PDBReader = new PDBReader(filename)

    while (pdbReader.hasNext) {
      parse_array += pdbReader.next().get
    }
  }

  override def toString : String = parse_array.mkString(", ")
  //TODO: should add method to add hydrogen atoms
  //TODO: should add method to remove water
}


class ChainIterator(input_sequence : Seq[PDBAtomInfo], chain : Char = 'L') extends Iterator[Seq[PDBAtomInfo]] {
  var tailSequence : Seq[PDBAtomInfo] = skipToChainStart(input_sequence, chain)

  def skipToChainStart(sequence : Seq[PDBAtomInfo], chain : Char) : Seq[PDBAtomInfo] = sequence match {
    case Seq() => Seq()
    case x if x.head.chainID == chain => x
    case x if x.head.chainID != chain => {
      skipToChainStart(x.tail, chain)
    }
  }

  def hasNext : Boolean = !tailSequence.isEmpty && tailSequence.head.chainID == chain

  def next() : Seq[PDBAtomInfo] = {
    var prefix : Seq[PDBAtomInfo] = Seq()
    val currentAA = tailSequence.head.resSeq
    val currentChain = tailSequence.head.chainID
    while (!tailSequence.isEmpty && tailSequence.head.chainID == currentChain && tailSequence.head.resSeq == currentAA) {
        prefix = prefix :+ tailSequence.head
        tailSequence = tailSequence.tail
    }
    prefix
  }
}

//TODO: should extend all logic of pdb file reading, the following class is temporary solution for collecting atoms to aminoacids
class PDBAminoAcidCollection(val basic_structure : PDBStructure) {
  val aminoacidsByChain = new ChainIterator(basic_structure.parse_array.toSeq)
  val aminoacids = basic_structure.parse_array.groupBy(_.chainID).map(x=>(x._1, x._2.groupBy(_.resSeq))).toMap
  val aminoacidIds = basic_structure.parse_array.groupBy(_.chainID).map(x=>(x._1, x._2.map(_.resSeq).distinct)).toMap

  val chains = aminoacids.keys
}
