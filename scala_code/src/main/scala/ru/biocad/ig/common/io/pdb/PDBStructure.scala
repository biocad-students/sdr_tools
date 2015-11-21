package ru.biocad.ig.common.io.pdb

import scala.collection.mutable.ArrayBuffer

//helper class, contains all information about atomic structure for given PDB
class PDBStructure {
  val parse_array : ArrayBuffer[PDBAtomInfo] = ArrayBuffer.empty[PDBAtomInfo]

  def readFile(filename : String) : Unit = {
    var pdbReader: PDBReader = new PDBReader(filename)
    parse_array ++= Stream.continually(pdbReader.next()).takeWhile(_ => pdbReader.hasNext).flatten
    pdbReader.close()
  }

  override def toString : String = parse_array.mkString(", ")
  //TODO: should add method to add hydrogen atoms
  //TODO: should add method to remove water
}
