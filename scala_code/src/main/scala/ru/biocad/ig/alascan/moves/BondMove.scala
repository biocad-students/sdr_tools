package ru.biocad.ig.alascan.moves

import ru.biocad.ig.common.structures.aminoacid.{SimplifiedAminoacid, SimplifiedChain}
import ru.biocad.ig.common.algorithms.geometry.AminoacidUtils
import ru.biocad.ig.alascan.constants.{AminoacidLibrary, SidechainInfo}
import util.Random.nextInt
import com.typesafe.scalalogging.slf4j.LazyLogging
import ru.biocad.ig.common.structures.geometry.{GeometryVector}

//TODO: add optional preprocessing and memorizing for allowed moves, if allowed, then optionally do it
/**there should be several kinds of bond makeMoves,
this class takes number of bonds to makeMove, starting from zero*/
class BondMove(val basicVectors :  Array[GeometryVector],
        val numberOfBonds : Int, val precompute : Boolean = false) extends LatticeBasicMove with LazyLogging {
  override val size = numberOfBonds - 1
  override val typeName = "BondMove"

  lazy val precomputedMoveSet : Map[Seq[Int], IndexedSeq[Seq[Int]]] = if (precompute) computeMoves() else Map()

  def computeMoves() : Map[Seq[Int], IndexedSeq[Seq[Int]]] = {
    val vectors = (0 to basicVectors.size - 1).toSeq
    Stream.continually(vectors).take(numberOfBonds - 1).foldLeft(vectors.map(Seq(_)).toSeq) ({
      case (result, vect) => result.map({
        subsequence => vect.map(_ +: subsequence)
        }).reduceLeft(_ ++ _)
    }).toIndexedSeq.groupBy({
      movesSequence => movesSequence.map(basicVectors(_)).reduceLeft(_ + _).roundedCoordinates.toSeq
    })
  }

  def getRandomVectors(n : Int) : Seq[GeometryVector] = Stream.continually(nextInt(basicVectors.size)).take(n).map(basicVectors(_))

  override def makeMove(structure : SimplifiedChain, position : Int) : SimplifiedChain = {
    logger.debug("in " + numberOfBonds.toString + "-BondMove starting at position: " + position.toString)
    if (precompute && position + numberOfBonds < structure.size) {
      //second term in condition is here to correctly compute bond move for chain ends, where last aminoacid is undefined
      val distance = structure.getDistance(position, numberOfBonds).roundedCoordinates
      val allowedMoves = precomputedMoveSet(distance)
      val moveVectors = allowedMoves(nextInt(allowedMoves.size)).map({basicVectors(_)}).init
      structure.moveFragment(moveVectors, position, numberOfBonds)
    }
    else {
      val moveVectors = getRandomVectors(numberOfBonds - 1)
      structure.moveFragment(moveVectors, position, numberOfBonds)
    }
  }

}
