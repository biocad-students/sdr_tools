package ru.biocad.ig.common.io.pdb

/** Iterates over particular aminoacid records in given chain
  *
  * @param input_sequence 'ATOM' recors from given PDB
  * @param chain chain to iterate
  */
class PDBChainIterator(input_sequence : Seq[PDBAtomInfo], chain : Char = 'A') extends Iterator[Seq[PDBAtomInfo]] {
  var tailSequence : Seq[PDBAtomInfo] = skipToChainStart(input_sequence, chain)

  /** @return sequence starting from record with chainID equal to chain parameter
    * if chainID == chain not found, returns empty sequence
    */
  def skipToChainStart(sequence : Seq[PDBAtomInfo], chain : Char) : Seq[PDBAtomInfo] = sequence match {
    case Seq() => Seq()
    case x if x.head.chainID == chain => x
    case x if x.head.chainID != chain => skipToChainStart(x.tail, chain)
  }

  def hasNext : Boolean = tailSequence.nonEmpty && tailSequence.head.chainID == chain

  /** @return PDBAtomInfo records corresponding to specific aminoacid in chain.
    */
  def next() : Seq[PDBAtomInfo] = {
    var prefix : Seq[PDBAtomInfo] = Seq()
    val currentAA = tailSequence.head.resSeq
    val currentChain = tailSequence.head.chainID
    while (tailSequence.nonEmpty && tailSequence.head.chainID == currentChain && tailSequence.head.resSeq == currentAA) {
        prefix = prefix :+ tailSequence.head
        tailSequence = tailSequence.tail
    }
    prefix
  }
}
