package ru.biocad.ig.common.io.pdb

import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import java.io.File

//helper class, contains all information about atomic structure for given PDB
class PDBStructure {
  val parse_array : ArrayBuffer[PDBAtomInfo] = ArrayBuffer.empty[PDBAtomInfo]

  def readFile(file : File) : Unit = {
    val fileSource = Source.fromFile(file)
    var pdbReader: PDBReader = new PDBReader(fileSource)
    try {
      parse_array ++= Stream.continually(pdbReader.next()).takeWhile(_ => pdbReader.hasNext).flatten
    }
    finally {
      pdbReader.close()
      fileSource.close()
    }
  }

  def readFile(filename : String) : Unit = readFile(new File(filename))

  def readFile(file : Source) : Unit = {
    var pdbReader: PDBReader = new PDBReader(file)
    try {
      parse_array ++= Stream.continually(pdbReader.next()).takeWhile(_ => pdbReader.hasNext).flatten
    }
    finally
      pdbReader.close()
  }

  override def toString : String = parse_array.mkString(", ")
  //TODO: should add method to add hydrogen atoms
  //TODO: should add method to remove water
}
