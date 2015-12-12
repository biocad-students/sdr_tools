package ru.biocad.ig.alascan.moves

import ru.biocad.ig.common.structures.aminoacid.{SimplifiedAminoacid, SimplifiedChain}
import ru.biocad.ig.common.algorithms.geometry.AminoacidUtils
import ru.biocad.ig.alascan.constants.{AminoacidLibrary, SidechainInfo}
import util.Random.nextInt
import com.typesafe.scalalogging.slf4j.LazyLogging

import ru.biocad.ig.common.structures.geometry.{GeometryVector}


//direction == 1.0 means towards N-terminus, direction = -1 means towards C
class DisplacementMove(val basicVectors :  Array[GeometryVector],
                       val rotamerLibrary: AminoacidLibrary[SidechainInfo]) extends LatticeBasicMove {
  def getRandomVector() : GeometryVector = basicVectors(nextInt(basicVectors.size))

  override val size = 1
  val rotamerMove = new RotamerMove(rotamerLibrary)

  def prepareMove(moveVector : GeometryVector,
      structure : SimplifiedChain,
      position : Int) : SimplifiedChain = {
    val movedStructure = structure.moveFragment(moveVector, position, structure.size)
    rotamerMove.makeMove(movedStructure, position)

  }

  override def makeMove(structure : SimplifiedChain, position : Int)
      : SimplifiedChain = prepareMove(getRandomVector(), structure, position)
}
