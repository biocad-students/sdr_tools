package ru.biocad.ig.common.structures.geometry

import ru.biocad.ig.common.structures.aminoacid.SimplifiedAminoAcid
import ru.biocad.ig.common.algorithms.geometry.AminoacidUtils
import ru.biocad.ig.alascan.constants.{AminoacidLibrary, SidechainInfo}
import util.Random.nextInt

trait LatticeBasicMove {
  def isValid() : Boolean = ???
  def makeMove(structure : Seq[SimplifiedAminoAcid], position : Int) : Seq[SimplifiedAminoAcid] = ???
}
/***/
class RotamerMove(val rotamerLibrary: AminoacidLibrary[SidechainInfo]) extends LatticeBasicMove {
  def moveRotamer(structure : Seq[SimplifiedAminoAcid], position : Int ) : SimplifiedAminoAcid = {
    val Seq(a1, a2, a3, a4) = Seq(position - 1, position, position+1, position + 2).map({i=>structure(i)})
    val (d1, d2, d3) = AminoacidUtils.getDistances(a1.ca, a2.ca, a3.ca, a4.ca)
    val (x, y, z) = AminoacidUtils.getLocalCoordinateSystem(a1.ca, a2.ca, a3.ca, a4.ca)
    val sidechainInfo = rotamerLibrary.restoreAminoAcidInfo(a2.name, d1, d2, d3)
    sidechainInfo.changeRotamerToRandom(a2)
  }
  override def makeMove(structure : Seq[SimplifiedAminoAcid], position : Int) : Seq[SimplifiedAminoAcid] = {
    println("in RotamerMove")
    structure.zipWithIndex.map({
      case (el, i) => {
        if (i == position)
        moveRotamer(structure, position)
        else el
      }
    })
  }
}

/**there should be several kinds of bond makeMoves,
this class takes number of bonds to makeMove, starting from zero*/
class BondMove(val basicVectors :  Array[GeometryVector], val numberOfBonds : Int) extends LatticeBasicMove {
  def prepareMove(moveVector : GeometryVector,
      structure : Seq[SimplifiedAminoAcid],
      position : Int) : Seq[SimplifiedAminoAcid] = {

    structure.zipWithIndex.map({case ( el, i) => {
      if (i >= position && i < position + numberOfBonds - 1)
      el.move(moveVector)
      else el
    }})

  }

  def getRandomVector() : GeometryVector = {
    basicVectors(nextInt(basicVectors.size))
  }

  override def makeMove(structure : Seq[SimplifiedAminoAcid],
    position : Int) : Seq[SimplifiedAminoAcid] = prepareMove(getRandomVector(), structure, position)
}

//direction == 1.0 means towards N-terminus, direction = -1 means tovards C
class DisplacementMove(val basicVectors :  Array[GeometryVector], val direction : Int) extends LatticeBasicMove {
  override def makeMove(structure : Seq[SimplifiedAminoAcid], position : Int) : Seq[SimplifiedAminoAcid] = {
    structure.zipWithIndex.map({case (el, i) => {
      if (i < position)
      el.move(Vector3d(1, 1, 1))
      else el
    }})
  }
}
