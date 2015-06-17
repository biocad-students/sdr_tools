package ru.biocad.ig.common.io.pdb

// All fields are named as stated in specification:
// ftp://ftp.wwpdb.org/pub/pdb/doc/format_descriptions/Format_v33_A4.pdf
case class PDBAtomInfo(
    serial : Int,
    atom : String,
    altLoc : Char,
    resName : String,
    chainID : Char,
    resSeq : String,
    iCode : Char,
    x : Double,
    y : Double,
    z : Double,
    occupancy : Double,
    tempFactor : Double,
    element : String,
    charge : String
  ) {
}

object PDBAtomInfo {
  def apply(line: String) : PDBAtomInfo = PDBAtomInfo(
    serial = line.substring(6, 11).trim.toInt,
    atom = line.substring(12, 16),
    altLoc = line.charAt(16),
    resName = line.substring(17, 20),
    chainID = line.charAt(21),
    resSeq = line.substring(22, 26),
    iCode = line.charAt(26),
    x = line.substring(30, 38).toDouble,
    y = line.substring(38, 46).toDouble,
    z = line.substring(46, 54).toDouble,
    occupancy = line.substring(54, 60).toDouble,
    tempFactor = line.substring(60, 66).toDouble,
    element = line.substring(76, 78),
    charge = line.substring(78, 80)
  )
}
