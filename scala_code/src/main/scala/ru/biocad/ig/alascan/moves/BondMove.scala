package ru.biocad.ig.alascan.moves

import ru.biocad.ig.common.structures.aminoacid.{SimplifiedAminoacid, SimplifiedChain}
import ru.biocad.ig.common.algorithms.geometry.AminoacidUtils
import ru.biocad.ig.alascan.constants.{AminoacidLibrary, SidechainInfo}
import util.Random.nextInt
import com.typesafe.scalalogging.slf4j.LazyLogging
import ru.biocad.ig.common.structures.geometry.{GeometryVector, Vector3d}
import ru.biocad.ig.alascan.constants.LatticeConstants

//TODO: add optional preprocessing and memorizing for allowed moves, if allowed, then optionally do it
/**there should be several kinds of bond makeMoves,
this class takes number of bonds to makeMove, starting from zero*/
class BondMove(val basicVectors :  Array[GeometryVector],
        val numberOfBonds : Int, val precompute : Boolean = false, val latticeConstants : Option[LatticeConstants] = None) extends LatticeBasicMove with LazyLogging {
  override val size = numberOfBonds - 1
  override val typeName = "BondMove"

  lazy val precomputedMoveSet : Map[Seq[Int], IndexedSeq[Seq[GeometryVector]]] = if (precompute) computeMoves() else Map()

  def validate(sequence : Seq[GeometryVector]) = latticeConstants match {
    case Some(constrains) => {
      if (sequence.length >= 2) {
        val v1 = sequence.head
        val v2 = sequence.tail.head
        constrains.checkAngleRestrictions(v2.angleTo(-v1)) && sequence.scanLeft(Vector3d(0, 0, 0) : GeometryVector)(_ + _).drop(3).forall(v1.distanceTo(_) >= constrains.caMinDistance/constrains.meshSize )
      }
      else {
        true
      }
    }
    case None => true//always valid if no constrains is given
  }
  def computeMoves() : Map[Seq[Int], IndexedSeq[Seq[GeometryVector]]] = {
    val vectors = basicVectors
    Stream.continually(vectors).take(numberOfBonds - 1).foldLeft(vectors.map(Seq(_))) ({
      case (result, vect) => result.map({
        subsequence => vect.map(_ +: subsequence).filter(validate(_))
        }).reduceLeft(_ ++ _)
    }).toIndexedSeq.groupBy({
      movesSequence => movesSequence.reduceLeft(_ + _).roundedCoordinates.toSeq
    })
  }

  def getRandomVectors(n : Int) : Seq[GeometryVector] = Stream.continually(nextInt(basicVectors.size)).take(n).map(basicVectors(_))

  override def makeMove(structure : SimplifiedChain, position : Int) : SimplifiedChain = {
    logger.debug("in " + numberOfBonds.toString + "-BondMove starting at position: " + position.toString)
    if (precompute && position + numberOfBonds < structure.size) {
      //second term in condition is here to correctly compute bond move for chain ends, where last aminoacid is undefined
      val distance = structure.getDistance(position, numberOfBonds).roundedCoordinates
      val allowedMoves = precomputedMoveSet(distance)
      val moveVectors = allowedMoves(nextInt(allowedMoves.size)).init
      structure.moveFragment(moveVectors, position, numberOfBonds)
    }
    else {
      val moveVectors = getRandomVectors(numberOfBonds - 1)
      structure.moveFragment(moveVectors, position, numberOfBonds)
    }
  }

}
