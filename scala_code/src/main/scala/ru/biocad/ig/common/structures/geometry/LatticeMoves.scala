package ru.biocad.ig.common.structures.geometry

import ru.biocad.ig.common.structures.aminoacid.SimplifiedAminoAcid

trait LatticeBasicMove {
  def isValid() : Boolean = ???
  def move(structure : Seq[SimplifiedAminoAcid], position : Int) : Seq[SimplifiedAminoAcid] = ???
}
/***/
class RotamerMove extends LatticeBasicMove {
  override def move(structure : Seq[SimplifiedAminoAcid], position : Int) : Seq[SimplifiedAminoAcid] = {
    structure.zipWithIndex.map({
      case (el, i) => {
        if (i == position)
        el.moveRotamer(Vector3d(1,1,1))
        else el
      }
    })
  }
}

/**there should be several kinds of bond moves,
this class takes number of bonds to move, starting from zero*/
class BondMove(val numberOfBonds : Int) extends LatticeBasicMove {
  override def move(structure : Seq[SimplifiedAminoAcid], position : Int) : Seq[SimplifiedAminoAcid] = {
    structure.zipWithIndex.map({case ( el, i) => {
      if (i >= position && i < position + numberOfBonds - 1)
      el.move(Vector3d(1,1,1)) //TODO: fix
      else el
    }})
  }
}

//direction == 1.0 means towards N-terminus, direction = -1 means tovards C
class DisplacementMove(val direction : Int) extends LatticeBasicMove {
  override def move(structure : Seq[SimplifiedAminoAcid], position : Int) : Seq[SimplifiedAminoAcid] = {
    structure.zipWithIndex.map({case (el, i) => {
      if (i < position)
      el.move(Vector3d(1, 1, 1))
      else el
    }})
  }

}
