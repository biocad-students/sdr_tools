package ru.biocad.ig.common.algorithms

import ru.biocad.ig.common.structures.geometry.LatticeBasicMove
import ru.biocad.ig.common.structures.aminoacid.SimplifiedAminoAcid
import scala.util.Random

object MonteCarloRunner{
  /** helper method to hide move attempt*/
  def attemptMove(currentStructure : Seq[SimplifiedAminoAcid],
    move : LatticeBasicMove,
    position : Int,
    getEnergy : Seq[SimplifiedAminoAcid] => Double) : Seq[SimplifiedAminoAcid] = {
      val newStructure = move.makeMove(currentStructure, position)
      val oldE = getEnergy(currentStructure)
      val newE = getEnergy(newStructure)
      if (oldE < newE) {
        newStructure
      }
      else {
        currentStructure
      }
  }
  def run(structure : Seq[SimplifiedAminoAcid],
          moves : Seq[LatticeBasicMove],
          getEnergy : Seq[SimplifiedAminoAcid] => Double,
          //ck_rule : Int => Double,
          numberOfMoves : Int = 1000) = {

    Stream.continually(
      (
        //Random.nextInt(structure.length),
        Random.shuffle(moves)
      )
    ).zipWithIndex.take(numberOfMoves).foldLeft(structure) {
      case (currentStructure, (shuffledMoves, time)) => {
        attemptMove(currentStructure, moves.head, 1, getEnergy)
      }

    }
  }
}
