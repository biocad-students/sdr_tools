package ru.biocad.ig.alascan.moves

import ru.biocad.ig.common.structures.aminoacid.{SimplifiedAminoacid, SimplifiedChain}
import ru.biocad.ig.common.algorithms.geometry.AminoacidUtils
import ru.biocad.ig.alascan.constants.{AminoacidLibrary, SidechainInfo}
import util.Random.nextInt
import com.typesafe.scalalogging.slf4j.LazyLogging

trait LatticeBasicMove {
  def makeMove(structure : SimplifiedChain, position : Int) : SimplifiedChain = ???
  def size : Int = ???
  val typeName = "BasicMove"
}
