package ru.biocad.ig.common.structures.geometry

import ru.biocad.ig.common.structures.aminoacid.{SimplifiedAminoacid, SimplifiedChain}
import ru.biocad.ig.common.algorithms.geometry.AminoacidUtils
import ru.biocad.ig.alascan.constants.{AminoacidLibrary, SidechainInfo}
import util.Random.nextInt
import com.typesafe.scalalogging.slf4j.LazyLogging

trait LatticeBasicMove {
  def makeMove(structure : SimplifiedChain, position : Int) : SimplifiedChain = ???
}
/***/
class RotamerMove(val rotamerLibrary: AminoacidLibrary[SidechainInfo])
        extends LatticeBasicMove with LazyLogging {
  def moveRotamer(structure : Seq[SimplifiedAminoacid], i : Int ) : SimplifiedAminoacid = {
    val v1 = i match {
      case 0 => structure(i + 1).ca - structure(i).ca
      case _ => structure(i).ca - structure(i - 1).ca
    }
    val v2 = i match {
      case i if i == structure.size - 1 => structure(i - 1).ca - structure(i - 2).ca
      case _ => structure(i + 1).ca - structure(i).ca
    }
    val v3 = i match {
      case i if i == structure.size - 1 => structure(i).ca - structure(i - 1).ca
      case i if i == structure.size - 2 => structure(i).ca - structure(i - 1).ca
      case _ => structure(i + 2).ca - structure(i + 1).ca
    }
    val aa = structure(i)
    val (d1, d2, d3) = AminoacidUtils.getDistances(v1, v2, v3)
    val (x, y, z) = AminoacidUtils.getLocalCoordinateSystem(v1, v2, v3)
    val sidechainInfo = rotamerLibrary.restoreAminoacidInfo(aa.name, d1, d2, d3)
    sidechainInfo.changeRotamerToRandom(aa)
  }

  override def makeMove(chain : SimplifiedChain, position : Int) : SimplifiedChain = {
    logger.debug("in RotamerMove at position: " + position.toString)

    val newAA = moveRotamer(chain.structure, position)
    chain.replaceRotamer(newAA.rotamer, position)
  }
}

/**there should be several kinds of bond makeMoves,
this class takes number of bonds to makeMove, starting from zero*/
class BondMove(val basicVectors :  Array[GeometryVector],
        val numberOfBonds : Int) extends LatticeBasicMove with LazyLogging {

  def prepareMove(moveVector : GeometryVector,
      structure : SimplifiedChain,
      position : Int) : SimplifiedChain = {
    logger.debug("in " + numberOfBonds.toString + "-BondMove starting at position: " + position.toString)
    structure.moveFragment(moveVector, position, numberOfBonds)
  }

  def getRandomVector() : GeometryVector = {
    basicVectors(nextInt(basicVectors.size))
  }

  override def makeMove(structure : SimplifiedChain,
    position : Int) : SimplifiedChain = prepareMove(getRandomVector(), structure, position)
}

//direction == 1.0 means towards N-terminus, direction = -1 means towards C
class DisplacementMove(val basicVectors :  Array[GeometryVector], val direction : Int) extends LatticeBasicMove {
  override def makeMove(structure : SimplifiedChain, position : Int) : SimplifiedChain = {
    structure.moveFragment(Vector3d(1, 1, 1), position, structure.size)
  }
}
