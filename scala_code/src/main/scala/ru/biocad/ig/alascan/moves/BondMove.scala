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
        val numberOfBonds : Int) extends LatticeBasicMove with LazyLogging {
  override val size = numberOfBonds - 1
  override val typeName = "BondMove"

  def prepareMove(moveVectors : Seq[GeometryVector],
      structure : SimplifiedChain,
      position : Int) : SimplifiedChain = {
    logger.debug("in " + numberOfBonds.toString + "-BondMove starting at position: " + position.toString)
    structure.moveFragment(moveVectors, position, numberOfBonds)
  }

  def getRandomVectors(n : Int) : Seq[GeometryVector] = Stream.continually(nextInt(basicVectors.size)).take(n).map(basicVectors(_))

  override def makeMove(structure : SimplifiedChain,
    position : Int) : SimplifiedChain = prepareMove(getRandomVectors(numberOfBonds - 1), structure, position)
}
