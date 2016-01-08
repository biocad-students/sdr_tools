package ru.biocad.ig.common.structures.aminoacid

import scala.collection._

import ru.biocad.ig.common.io.pdb.PDBAtomInfo
import ru.biocad.ig.common.structures.geometry.{Lattice, Vector3d, GeometryVector}
import ru.biocad.ig.alascan.moves.RotamerMove
import ru.biocad.ig.alascan.constants.{AminoacidLibrary, SidechainInfo}

/** hides sequence of simplified aminoacids and constructs them from various sources
  */
case class SimplifiedChain(val structure : Array[SimplifiedAminoacid], val lattice : Lattice) extends Traversable[SimplifiedAminoacid] {
  /**differences between consecutive Ca atoms in lattice units */
  val vectors : Seq[GeometryVector] = (structure zip structure.tail).map({case (x, y) => y.caInLatticeCoordinates - x.caInLatticeCoordinates})

  override val size = structure.size

  def apply(index : Int) : SimplifiedAminoacid = structure(index)

  def foreach[U](f: SimplifiedAminoacid => U) = structure.foreach(f)

  def replaceAminoacid(positionFunc : (Int) => Boolean,
                       replaceFunction : (SimplifiedAminoacid, Int) => SimplifiedAminoacid
                      ) : SimplifiedChain = {
    new SimplifiedChain(structure.zipWithIndex.map({
      case (el, i) => if (positionFunc(i)) replaceFunction(el, i) else el
    }), lattice)
  }

  def replaceRotamer(newRotamer : GeometryVector, position : Int) : SimplifiedChain = {
    replaceAminoacid({_ == position}, {case (aa, _) => aa.changeRotamerTo(newRotamer) })
  }

  def moveFragment(moveVectors : Seq[GeometryVector], position : Int, numberOfBonds : Int) : SimplifiedChain = {
    val deltaVectors = (moveVectors zip vectors.drop(position - 1)).map({
      case (a, b) => a - b
    })
    replaceAminoacid({i => i > position && i < position + numberOfBonds - 1},
      { case (aa, index) => aa.move(deltaVectors(index - position)) })
  }

  def moveFragment(moveVector : GeometryVector, position : Int, numberOfBonds : Int) : SimplifiedChain = {
    val deltaVector = moveVector - vectors(position)
    replaceAminoacid({i => i > position && i < position + numberOfBonds - 1},
      { case (aa, index) => aa.move(deltaVector) })
  }

  /** helper method - for BondMove
    * @return distance as vector of lattice units between given aminoacids
    */
  def getDistance(position : Int, numberOfBonds : Int) : GeometryVector = {
    structure(position + numberOfBonds).caInLatticeCoordinates - structure(position).caInLatticeCoordinates
  }

  /** represents part of one alanine scanning step -
    * performs substitution at given position with new aminoacid, and returns modified chain.
    * seems to be convenience method, as most of this is made in moveRotamer.
    * only rotamer and aminoacid name should be replaced here.
    * slightly differs from replaceRotamer - aminoacid's name also gets changed
    */
  def mutateAtPoint(newAminoacidName : String, position : Int, newRotamer : GeometryVector) : SimplifiedChain = {
    replaceAminoacid({_ == position},
      { case (aa, _) => SimplifiedAminoacid(newAminoacidName, aa.ca, newRotamer, aa.latticeSize) })
  }

  /*
  return value indicates that aminoacids i and j are in contact
  array can be aminoacid-pair specific.
  now there is no KDTree, just simple and slow code - should replace it
  */
  private def buildContactMap() : Array[Seq[Int]] = {
    structure.zipWithIndex.map({case (aa1, i1) => {
      (0 to size - 1).filter({
        i2 => {
          val aa2 = structure(i2)
          aa2.isInContactWith(aa1, lattice.rotamerRadiusInfo.getR(aa1.name, aa2.name, lattice.latticeConstants.meshSize))
          }
      })
    } }).toArray
  }

  val contactMap = buildContactMap()
}

object SimplifiedChain {
  def apply(originalSequence : Seq[Seq[PDBAtomInfo]], lattice : Lattice) = {
    new SimplifiedChain(originalSequence.map(SimplifiedAminoacid(_, lattice.latticeConstants.meshSize)).toArray, lattice)
  }

  val aa3letter = Map('A' -> "ALA", 'R' -> "ARG",
    'N' -> "ASN", 'D' -> "ASP", 'C' -> "CYS", 'Q' -> "GLN",
    'E' -> "GLU", 'G' -> "GLY", 'H' -> "HIS", 'I' -> "ILE",
    'L' -> "LEU", 'K' -> "LYS", 'M' -> "MET", 'F' -> "PHE",
    'P' -> "PRO", 'S' -> "SER", 'T' -> "THR", 'W' -> "TRP",
    'Y' -> "TYR", 'V' -> "VAL"
  )


  /** should build original simplified chain from given string with aminoacid 1-letter names.
    * Original chain is formed as coiled coil.
    * @param sequence string with sequence of aminoacid names in 1-letter IUPAC codes
    * @param rotamerLibrary reference rotamerLibrary to find rotamers for given aminoacid types
    * @return simplified, unfolded, valid structure with alpha-carbons and rotamers information
    */
  def fromSequence(sequence : String, lattice : Lattice) : SimplifiedChain = {
    val d : Seq[String] = sequence.flatMap(aa3letter.get(_))
    val vectors = lattice.prepareValidVectors(d.size - 1).scanLeft(Vector3d(0, 0, 0) : GeometryVector) (_ + _)
    val m = new RotamerMove(lattice.sidechainsInfo)
    val s1 = (d, vectors).zipped.map({case (aaName, ca) =>
      new SimplifiedAminoacid(aaName, ca*lattice.latticeConstants.meshSize, Vector3d(0, 0, 0), lattice.latticeConstants.meshSize)})
    val s2 = s1.zipWithIndex.map({case (aa, i) =>
        aa.changeRotamerTo(m.moveRotamer(s1, i))
    }).toArray
    new SimplifiedChain(s2, lattice)
    //1. generate
  }

}
