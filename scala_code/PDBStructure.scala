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
}
