package ru.biocad.ig.common.structures.aminoacid

import ru.biocad.ig.common.io.pdb.{PDBAtomInfo}
import ru.biocad.ig.alascan.constants.LatticeConstants
import ru.biocad.ig.alascan.constants.{SidechainInfo}

import ru.biocad.ig.common.structures.geometry._
import com.typesafe.scalalogging.slf4j.LazyLogging

/** Represents simplified model of aminoacid.
  * It consists of C_\alpha atom (with coordinates placed on lattice) and united atom (with off-lattice coordinates)
  *
  * @param name aminoacid 3-letter IUPAC identity name (like ALA, GLU, etc.)
  * @param ca coordinates of C_\alpha center, defined in lattice units
  * @param rotamer vector from C_\alpha to center of united atom (which appears to be center of mass for sidechain) in off-lattice coordinates
  * @param latticeSize lattice mesh size if aminoacid is projected to some, otherwise 1.0
  */
case class SimplifiedAminoacid(val name : String,
                               val ca : GeometryVector,
                               val rotamer : GeometryVector,
                               val latticeSize : Double) {
  val caInLatticeCoordinates = (ca / latticeSize).round

  //TODO : check if it is useful
  def getUpdatedAtomInfo(atom : String, updatedCoordinates : GeometryVector,
          atomsMap : Map[String, PDBAtomInfo]) : PDBAtomInfo = updatedCoordinates match {
      case Vector(Seq(x, y, z)) => {
          val a = atomsMap(atom)
          new PDBAtomInfo(a.serial, a.atom, a.altLoc, a.resName, a.chainID,
            a.resSeq, a.iCode, x, y, z, a.occupancy, a.tempFactor, a.segmentID, a.element, a.charge)
      }
  }

  def isInContactWith(aa : SimplifiedAminoacid, distance_cutoff : Double = 4.2) : Boolean = {
    // when we use reduced representation, we can compute 'contact' between rotamer's centers of masses
    (rotamer - aa.rotamer).length < distance_cutoff
  }

  override def toString = Seq(name, "ca: " + ca.toString,
    "rotamer: " + rotamer.toString).mkString("SimplifiedAminoacid{", ",  ", "}")

  /** moves CA atom to shift vector given in lattice units
    *
    * @param shift shift vector given in lattice units
    * @return new aminoacid with `caInLatticeCoordinates` value shifted by `shift` vector
    */
  def move(shift : GeometryVector) : SimplifiedAminoacid = new SimplifiedAminoacid(name, ca + shift * latticeSize, rotamer, latticeSize)

  def changeRotamerTo(newRotamer : GeometryVector) = new SimplifiedAminoacid(name, ca, newRotamer, latticeSize)
}

object SimplifiedAminoacid extends LazyLogging {

  //FIX: ..
  /** there should be rotamer center+radius.
  If rotamer center is changed and ca moved,  we should recompute all atoms when moving back to full-atom model.
  If there were no changes, we can probably use original coordinates of atoms.
  */
  //rotamer has off-lattice coordinates vs. Ca's are projected onto lattice
  //TODO: fix this (ca, should be center of masses)
  private def computeRotamerCenterCoordinates(atomsMap : Map[String, PDBAtomInfo], ca : GeometryVector) : GeometryVector = {
    logger.info("in computeCenterCoordinates")
    val rotamerAtoms  = atomsMap.filterKeys(!Set("N", "H", "C", "O").contains(_)).values.map(_.toVector - ca)
    //note: "CA" atoms is accounted in computed rotamer position, as stated in [Feig at al.], that's why it is not excluded
    rotamerAtoms.size match {
      case 0 => Vector3d(0, 0, 0)
      case n => rotamerAtoms.reduceLeft(_ + _) / n
    }
  }

  def apply(atoms : Seq[PDBAtomInfo], latticeSize : Double = 1.0) = {
    val name : String = if (atoms.size > 0) atoms.head.resName else ""
    val atomsMap = atoms.map(atom => atom.atom -> atom).toMap
    val ca : GeometryVector = atomsMap("CA").toVector

    new SimplifiedAminoacid(name, ca, computeRotamerCenterCoordinates(atomsMap, ca), latticeSize)
  }

  def getUpdatedAtomInfo(updatedCoordinates : GeometryVector,
    a : PDBAtomInfo) : PDBAtomInfo = updatedCoordinates match {
      case Vector(Seq(x, y, z)) => new PDBAtomInfo(a.serial, a.atom, a.altLoc, a.resName, a.chainID,
        a.resSeq, a.iCode, x, y, z, a.occupancy, a.tempFactor, a.segmentID, a.element, a.charge)
  }
}
