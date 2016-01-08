package ru.biocad.ig.alascan.moves

import ru.biocad.ig.common.structures.aminoacid.{SimplifiedAminoacid, SimplifiedChain}
import ru.biocad.ig.common.algorithms.geometry.AminoacidUtils
import ru.biocad.ig.alascan.constants.{AminoacidLibrary, SidechainInfo}
import util.Random.nextInt
import com.typesafe.scalalogging.slf4j.LazyLogging

import ru.biocad.ig.common.structures.geometry.{Vector3d, GeometryVector}

/***/
class RotamerMove(val rotamerLibrary: AminoacidLibrary[SidechainInfo])
        extends LatticeBasicMove with LazyLogging {

  def moveRotamer(structure : Seq[SimplifiedAminoacid], i : Int) : GeometryVector = moveRotamer(structure, i, structure(i).name)
  //TODO: change this to call structure.vectors
  def moveRotamer(structure : Seq[SimplifiedAminoacid], i : Int, aaName : String ) : GeometryVector = {
    val v1 = i match {
      case 0 => structure(i + 1).caInLatticeCoordinates - structure(i).caInLatticeCoordinates
      case _ => structure(i).caInLatticeCoordinates - structure(i - 1).caInLatticeCoordinates
    }
    val v2 = i match {
      case i if i == structure.size - 1 => structure(i - 1).caInLatticeCoordinates - structure(i - 2).caInLatticeCoordinates
      case _ => structure(i + 1).caInLatticeCoordinates - structure(i).caInLatticeCoordinates
    }
    val v3 = i match {
      case i if i == structure.size - 1 => structure(i).caInLatticeCoordinates - structure(i - 1).caInLatticeCoordinates
      case i if i == structure.size - 2 => structure(i).caInLatticeCoordinates - structure(i - 1).caInLatticeCoordinates
      case _ => structure(i + 2).caInLatticeCoordinates - structure(i + 1).caInLatticeCoordinates
    }
    val aa = structure(i)
    val (d1, d2, d3) = AminoacidUtils.getDistances(v1, v2, v3)
    val (x, y, z) = AminoacidUtils.getLocalCoordinateSystem(v1, v2, v3)
    rotamerLibrary.restoreAminoacidInfo(aaName, d1, d2, d3) match {
      case Some(sidechainInfo) => sidechainInfo.changeRotamerToRandom(aa.rotamer, x, y, z)
      case None => Vector3d(0, 0, 0) //FIX: should decide if it shouldn't be zero
    }

  }

  override val size = 0


  override def makeMove(chain : SimplifiedChain, position : Int) : SimplifiedChain = position match {
    case position if position < 0 || position >= chain.size => chain
    case _ => {
      logger.debug("in RotamerMove at position: " + position.toString)
      val newRotamer = moveRotamer(chain.structure, position)
      chain.replaceRotamer(newRotamer, position)
    }
  }
}
