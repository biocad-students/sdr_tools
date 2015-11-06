package ru.biocad.ig.common.algorithms

import ru.biocad.ig.common.structures.aminoacid.{SimplifiedAminoacid, SimplifiedChain}
import ru.biocad.ig.common.structures.geometry.Lattice
import scala.util.Random
import com.typesafe.scalalogging.slf4j.LazyLogging
import ru.biocad.ig.alascan.moves._

object MonteCarlo extends LazyLogging {
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
            logger.debug("%d, %s".format(currentStructure.size - move.size, move.typeName))
            if (currentStructure.size - move.size < 0)
              currentStructure
            else{
              val position = Random.nextInt(currentStructure.size - move.size)
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
}
