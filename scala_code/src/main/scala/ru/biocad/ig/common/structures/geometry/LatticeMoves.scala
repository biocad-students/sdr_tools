package ru.biocad.ig.common.structures.geometry

import ru.biocad.ig.common.structures.aminoacid.SimplifiedAminoAcid

trait LatticeBasicMove {
  def isValid() : Boolean = ???
  def move(structure : Seq[SimplifiedAminoAcid], position : Int) : Seq[SimplifiedAminoAcid] = ???
}
/***/
class RotamerMove extends LatticeBasicMove {

}

/**there should be several kinds of bond moves,
this class takes number of bonds to move, starting from zero*/
class BondMove(val numberOfBonds : Int) extends LatticeBasicMove {

}

class DisplacementMove extends LatticeBasicMove {

}
