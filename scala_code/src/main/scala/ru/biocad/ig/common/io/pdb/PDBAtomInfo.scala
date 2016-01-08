package ru.biocad.ig.common.io.pdb

import ru.biocad.ig.common.structures.geometry._


/** Represents Protein Data Bank's 1 'ATOM' section's line.
  *
  * All fields are named as stated in specification:
  * ftp://ftp.wwpdb.org/pub/pdb/doc/format_descriptions/Format_v33_A4.pdf
  */
case class PDBAtomInfo(
    serial : Int,
    atom : String,
    altLoc : Char,
    resName : String,
    chainID : Char,
    resSeq : Int, //String,//FIX: there are cases when resSeq contains letter, i.e. '100B'
    iCode : Char,
    x : Double,
    y : Double,
    z : Double,
    occupancy : Double,
    tempFactor : Double,
    segmentID : String,
    element : String,
    charge : String
  ) {

    def toVector : GeometryVector = Vector3d(x, y, z)

  /** this method can be redefined if needed.
    * If you need PDB 'ATOM' string as stated in Protein Data Bank Format specification v.4, use serialize() method.
    * @return some PDB atom info string representation
    */
  override def toString = serialize()

  /** @return corresponding PDB 'ATOM' string, as stated in Protein Data Bank Format specification.
    */
  def serialize() = {
    "ATOM  %5d %4s%c%3s %c%4d%c   %8.3f%8.3f%8.3f%6.2f%6.2f      %-4s%2s%2s".format(productIterator.toSeq : _*)
  }
}

object PDBAtomInfo {
  def apply(line: String) : PDBAtomInfo = PDBAtomInfo(
    /*serial =*/ line.substring(6, 11).trim.toInt,
    /*atom =*/ line.substring(12, 16).trim,
    /*altLoc =*/ line.charAt(16),
    /*resName =*/ line.substring(17, 20),
    /*chainID =*/ line.charAt(21),
    /*resSeq =*/ line.substring(22, 26).trim.toInt,
    /*iCode =*/ line.charAt(26),
    /*x =*/ line.substring(30, 38).toDouble,
    /*y =*/ line.substring(38, 46).toDouble,
    /*z =*/ line.substring(46, 54).toDouble,
    /*occupancy =*/ line.substring(54, 60).toDouble,
    /*tempFactor =*/ line.substring(60, 66).toDouble,
    /*segmentID =*/ line.substring(72, 76).trim,
    /*element =*/ line.substring(76, 78).trim,
    /*charge =*/ line.substring(78, 80).trim
  )

  def apply(index : Int, atomName : String, aaName : String, chainName : Char, resID: Int,
    v : GeometryVector) : PDBAtomInfo = v match {
      case Vector(Seq(x, y, z)) => {
          new PDBAtomInfo(index, atomName, ' ', aaName, chainName, resID,
          ' ', x, y, z, 0.0, 0.0, "", atomName.substring(0, 1), "" )
        }
    }
}
