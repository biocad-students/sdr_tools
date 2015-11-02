package ru.biocad.ig.common.algorithms

import ru.biocad.ig.common.structures.geometry.LatticeBasicMove
import ru.biocad.ig.common.structures.aminoacid.{SimplifiedAminoacid, SimplifiedChain}
import ru.biocad.ig.common.structures.geometry.Lattice
import scala.util.Random

object MonteCarlo{
  /** helper method to hide move attempt*/
  def attemptMove(currentStructure : SimplifiedChain,
    move : LatticeBasicMove,
    position : Int,
    getEnergy : SimplifiedChain => Double) : SimplifiedChain = {
      val newStructure = move.makeMove(currentStructure, position)
      val oldE = getEnergy(currentStructure)
      val newE = getEnergy(newStructure)
      //println("in attemptMove: ")
      //println(newE)
      if (oldE < newE) {
        //println("accept new")
        newStructure
      }
      else {
        //println("reject new")
        currentStructure
      }
  }
  def run(structure : SimplifiedChain,
          moves : Seq[LatticeBasicMove],
          getEnergy : SimplifiedChain => Double,
          //ck_rule : Int => Double,
          numberOfMoves : Int = 1000) = {

    Stream.continually(
      (
        //Random.nextInt(structure.length),
        Random.shuffle(moves)
      )
    ).zipWithIndex.take(numberOfMoves).foldLeft(structure) {
      case (currentStructure, (shuffledMoves, time)) => {
        shuffledMoves.foldLeft(currentStructure) {
          case (current, move) => {
            val position = Random.nextInt(structure.size - move.size)
            val newStructure = attemptMove(currentStructure, move, position, getEnergy)
            if (Lattice.validateStructure(newStructure))
              newStructure
            else currentStructure
          }
        }

      }

    }
  }
}
