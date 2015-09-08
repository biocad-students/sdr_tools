package ru.biocad.ig.common.structures.aminoacid

import ru.biocad.ig.common.io.pdb.{PDBAtomInfo}

import ru.biocad.ig.common.structures.geometry._

class SimplifiedAminoAcid(val atoms : Seq[PDBAtomInfo]) {
  val name = atoms.head.resName
  val atomsMap = atoms.map(atom => atom.atom -> atom).toMap
  var ca : GeometryVector = Vector3d(atomsMap("CA").x, atomsMap("CA").y, atomsMap("CA").z)
  val atomsVectorMap = atomsMap.map(x=> (x._1, Vector3d(x._2.x, x._2.y, x._2.z) - ca))

  //FIX: ..
  /** there should be rotamer center+radius.
  If rotamer center is changed and ca moved,  we should recompute all atoms when moving back to full-atom model.
  If there were no changes, we can probably use original coordinates of atoms.
  */
  //rotamer has off-lattice coordinates vs. Ca's are projected onto lattice
  //TODO: fix this (ca, should be center of masses)
  def computeCenterCoordinates(atoms : Seq[PDBAtomInfo]) : GeometryVector = {
    val rotamerAtoms  = atoms.filterNot({
        s => Seq("N", "H", "CA", "C", "O").contains(s.atom)
      }).map({
        a => Vector3d(a.x, a.y, a.z) - ca
        })
    val center = rotamerAtoms.size match {
      case 0 => Vector3d(0, 0, 0)
      case n => rotamerAtoms.reduceLeft(_ + _) / n
    }
    center
  }

  var rotamer : Rotamer = new Rotamer(atomsVectorMap, computeCenterCoordinates(atoms)) //
  //TODO: should add relative coords, filter atoms to include only rotamer atoms

  def isInContactWith(aa : SimplifiedAminoAcid, distance_cutoff : Double = 4.2) : Boolean = {
    /*atoms.forall({case atom => {
      val atom_vector = Vector3d(atom.x, atom.y, atom.z)
      val len = aa.atoms.map({case atom2 => (atom_vector - Vector3d(atom2.x, atom2.y, atom2.z)).length}).min
      len > distance_cutoff
    }})*/
    // when we use reduced representation, we can compute 'contact' between rotamer's centers of masses
    (rotamer.center - aa.rotamer.center).length < distance_cutoff
    //return false
    //TODO: should implement
  }

  override def toString = Seq(name,
    //",",
    //"original atoms: ",
    //atoms.mkString("\n[ \n  ", ", \n  ", " ],"),
    "ca: ",
    ca.toString + ",",
    "\nrotamer: ",
    rotamer.toString
  ).mkString("SimplifiedAminoacid{\n", "\n ", "\n}")

  def move(shift : GeometryVector) : SimplifiedAminoAcid = {
    val result = new SimplifiedAminoAcid(atoms)
    result.ca = this.ca + shift
    result
  }

  def moveRotamer(shift : GeometryVector) : SimplifiedAminoAcid = ???
}
