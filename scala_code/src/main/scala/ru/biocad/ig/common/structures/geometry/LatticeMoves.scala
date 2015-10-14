package ru.biocad.ig.common.structures.geometry

import ru.biocad.ig.common.structures.aminoacid.SimplifiedAminoAcid
import util.Random.nextInt

trait LatticeBasicMove {
  def isValid() : Boolean = ???
  def makeMove(structure : Seq[SimplifiedAminoAcid], position : Int) : Seq[SimplifiedAminoAcid] = ???
}
/***/
class RotamerMove extends LatticeBasicMove {
  override def makeMove(structure : Seq[SimplifiedAminoAcid], position : Int) : Seq[SimplifiedAminoAcid] = {
    structure.zipWithIndex.map({
      case (el, i) => {
        if (i == position)
        el.moveRotamer(Vector3d(1,1,1))
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
