package ru.biocad.ig.common.structures.geometry

import ru.biocad.ig.common.io.pdb.{PDBStructure, PDBAtomInfo, PDBAminoAcidCollection}

//class LatticeVector = Seq[Int]
/** should represent basic general lattice with given meshSize and set of basic vectors.
  * should convert real pdb coordinates to lattice coordinates and vice versa,
  * handle conversion to different lattice coordinates.
*/
/*class Lattice(val meshSize : Double, val basicVectors: Seq[Seq[Int]]) {

}*/

class Rotamer(val atoms : Seq[PDBAtomInfo], val ca : GeometryVector) {
  val sidechainRelativeVectors = atoms.map({atom=> Vector3d(atom.x, atom.y, atom.z) - ca})
  val center : GeometryVector = if (sidechainRelativeVectors.size > 0)
      sidechainRelativeVectors.reduceLeft (_ + _) / sidechainRelativeVectors.size 
    else Vector3d(0,0,0)

  override def toString = sidechainRelativeVectors.mkString("[[", ",\n", "]]") + "\n, center = " + center.toString
}

class SimplifiedAminoAcid(val atoms : Seq[PDBAtomInfo]) {
  val ca : GeometryVector = atoms.find{_.atom.trim == "CA"} match{
    case Some(value) => Vector3d(value.x, value.y, value.z)
    case None => Vector3d(0.0, 0.0, 0.0)
    }
  //FIX: ..
  /** there should be rotamer center+radius.
  If rotamer center is changed and ca moved,  we should recompute all atoms when moving back to full-atom model.
  If there were no changes, we can probably use original coordinates of atoms.
  */
  //rotamer has off-lattice coordinates vs. Ca's are projected onto lattice
  var rotamer : Rotamer = new Rotamer(atoms.filterNot{s=>Seq("N", "H", "CA", "C", "O").contains(s.atom.trim)}, ca)//todo: check this
  def isInContactWith(aa : SimplifiedAminoAcid, distance_cutoff : Double = 4.2) : Boolean = {
    atoms.forall({case atom => {
      val atom_vector = Vector3d(atom.x, atom.y, atom.z)
      val len = aa.atoms.map({case atom2 => (atom_vector - Vector3d(atom2.x, atom2.y, atom2.z)).length}).min
      len > distance_cutoff
    }})
    //return false
    //TODO: should implement
  }

  override def toString = Seq(
    "original atoms: ",
    atoms.mkString("\n[ \n  ", ", \n  ", " ],"),
    "\nca: ",
    ca.toString + ",",
    "\nrotamer: ",
    rotamer.toString
  ).mkString("SimplifiedAminoacid{\n", "\n ", "\n}")
}
