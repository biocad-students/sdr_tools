package ru.biocad.ig.common.structures.aminoacid

import ru.biocad.ig.common.io.pdb.PDBAtomInfo

import ru.biocad.ig.common.structures.geometry._

class Rotamer(val atoms : Seq[PDBAtomInfo], val ca : GeometryVector) {
  val sidechainRelativeVectors = atoms.map({atom=> Vector3d(atom.x, atom.y, atom.z) - ca})
  val center : GeometryVector = if (sidechainRelativeVectors.size > 0)
      sidechainRelativeVectors.reduceLeft (_ + _) / sidechainRelativeVectors.size
    else Vector3d(0,0,0)

  override def toString = sidechainRelativeVectors.mkString("[[",
    ",\n", "]]") + "\n, center = " + center.toString
}
