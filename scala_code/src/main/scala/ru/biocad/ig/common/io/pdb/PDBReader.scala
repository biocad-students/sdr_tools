package ru.biocad.ig.common.io.pdb

import scala.io.Source
import ru.biocad.ig.common.io.common.SourceReader

/** Reads file in Protein Data Bank format v.4.
  * Currently only 'ATOM' section is processed, anything else is ignored.
  * I mean, ANYTHING. It's importaint!
  * Because PDB file can contain:
  * - several structures with several chains,
  * - waters (in atom section they are often marked as 'HOH' in resName field), PTMs
  * - missing atoms
  * - wrong data (i.e., atom from next aminoacid can be marked with the same resSeq number as atoms in previous aminoacid)
  * - etc. (I mentioned here only things I've actually seen in PDB files, there can be other things in it)
  */
class PDBReader(source : Source) {
  private val reader = new SourceReader(source)
  private var nextAtomString : String = skipToNext()

  def hasNext : Boolean = nextAtomString != null

  def next() : Option[PDBAtomInfo] = {
    if (hasNext) {
      val ai : PDBAtomInfo =  PDBAtomInfo(nextAtomString)
      nextAtomString = skipToNext()
      Some(ai)
    } else {
      None
    }
  }

  def close() : Unit = reader.close()

  private def skipToNext() : String = {
    var line : String = ""
    reader.synchronized {
      line = reader.readLine()
      while (line != null && line.substring(0, 4) != "ATOM") {
        line = reader.readLine()
      }
    }
    line
  }
}
