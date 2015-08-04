package ru.biocad.ig.common.structures.geometry

import ru.biocad.ig.common.io.pdb.{PDBStructure, PDBAtomInfo, PDBAminoAcidCollection}

//class LatticeVector = Seq[Int]
/** should represent basic general lattice with given meshSize and set of basic vectors.
  * should convert real pdb coordinates to lattice coordinates and vice versa,
  * handle conversion to different lattice coordinates.
*/
/*class Lattice(val meshSize : Double, val basicVectors: Seq[Seq[Int]]) {

}*/

class Rotamer(val atoms : Seq[PDBAtomInfo], val ca : PDBAtomInfo) {
  val center = atoms.foldLeft(Vector3d(0,0,0)) ({case (result, atom) => {
    //todo: add weight to atom center determination
    result + Vector3d(atom.x, atom.y, atom.z)
  }})/atoms.size
  val center_relative = center //todo: add relativity

}

class SimplifiedAminoAcid(val atoms : Seq[PDBAtomInfo]) {
  val ca = atoms.find{_.atom == "CA"}
  /** there should be rotamer center+radius.
  If rotamer center is changed and ca moved,  we should recompute all atoms when moving back to full-atom model.
  If there were no changes, we can probably use original coordinates of atoms.
  */
  //rotamer has off-lattice coordinates vs. Ca's are projected onto lattice
  var rotamer : Rotamer = new Rotamer(atoms.filterNot{_.atom in Seq("N", "H")})//todo: check this
  def isInContactWith(aa : SimplifiedAminoAcid) : Boolean = {
    return false
    //TODO: should implement
  }
}
