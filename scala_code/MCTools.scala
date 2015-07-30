package ru.biocad.ig.common.structures.geometry

import ru.biocad.ig.common.io.pdb.{PDBStructure, PDBAtomInfo, PDBAminoAcidCollection}

//class LatticeVector = Seq[Int]
/** should represent basic general lattice with given meshSize and set of basic vectors.
  * should convert real pdb coordinates to lattice coordinates and vice versa,
  * handle conversion to different lattice coordinates.
*/
class Lattice(val meshSize : Double, val basicVectors: Seq[Seq[Int]]) {

}

class SimplifiedAminoAcid(val atoms : Seq[PDBAtomInfo]) {
  val ca = atoms.find{_.atom == "CA"}
  
}
