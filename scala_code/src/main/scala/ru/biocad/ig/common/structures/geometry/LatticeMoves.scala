package ru.biocad.ig.common.structures.geometry

trait LatticeBasicMove {
  def isValid() : Boolean = ???
  def move() : Unit = ???
}
/***/
class RotamerMove extends LatticeBasicMove {

}

/**there should be several kinds of bond moves,
this class takes number of bonds to move, starting from zero*/
class BondMove(val numberOfBonds) extends LatticeBasicMove {

}

class DisplacementMove extends LatticeBasicMove {

}
