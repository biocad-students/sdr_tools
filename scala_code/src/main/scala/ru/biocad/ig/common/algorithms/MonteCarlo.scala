package ru.biocad.ig.common.algorithms

import ru.biocad.ig.common.structures.geometry.LatticeBasicMove
import ru.biocad.ig.common.structures.aminoacid.SimplifiedAminoacid
import ru.biocad.ig.common.structures.geometry.Lattice
import scala.util.Random

object MonteCarloRunner{
  /** helper method to hide move attempt*/
  def attemptMove(currentStructure : Seq[SimplifiedAminoacid],
    move : LatticeBasicMove,
    position : Int,
    getEnergy : Seq[SimplifiedAminoacid] => Double) : Seq[SimplifiedAminoacid] = {
      val newStructure = move.makeMove(currentStructure, position)
      val oldE = getEnergy(currentStructure)
      val newE = getEnergy(newStructure)
      println("in attemptMove: ")
      println(newE)
      if (oldE < newE) {
        println("accept new")
        newStructure
      }
      else {
        println("reject new")
        currentStructure
      }
  }
  def run(structure : Seq[SimplifiedAminoacid],
          moves : Seq[LatticeBasicMove],
          getEnergy : Seq[SimplifiedAminoacid] => Double,
          //ck_rule : Int => Double,
          numberOfMoves : Int = 1000) = {

    Stream.continually(
      (
        //Random.nextInt(structure.length),
        Random.shuffle(moves)
      )
    ).zipWithIndex.take(numberOfMoves).foldLeft(structure) {
      case (currentStructure, (shuffledMoves, time)) => {
        val newStructure = attemptMove(currentStructure, moves.head, 1, getEnergy)
        if (Lattice.validateStructure(newStructure))
          newStructure
        else currentStructure
      }

    }
  }
}
