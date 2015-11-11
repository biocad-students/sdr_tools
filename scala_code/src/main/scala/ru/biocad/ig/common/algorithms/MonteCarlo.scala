package ru.biocad.ig.common.algorithms

import ru.biocad.ig.common.structures.aminoacid.{SimplifiedAminoacid, SimplifiedChain}
import ru.biocad.ig.common.structures.geometry.Lattice
import scala.util.Random
import com.typesafe.scalalogging.slf4j.LazyLogging
import ru.biocad.ig.alascan.moves._

object MonteCarlo extends LazyLogging {
  /** helper method, hides one move attempt in MC run
    *
    * currently structure validation is done elsewhere
    */
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

  /** Starts MC algorithm and returns improved structure
    *
    * @param structure immutable object to process, represent current protein state.
    * @param moves sequence of different types of moves with approximate number of times each of them should appear in one MC time unit.
    * one MC time unit is defined as a sum of that numbers.
    * @param getEnergy function to compute energies in MC steps
    * @param mcTimeUnits number of MC time units
    */
  def run(structure : SimplifiedChain,
          moves : Seq[(LatticeBasicMove, Int)],
          getEnergy : SimplifiedChain => Double,
          //ck_rule : Int => Double,
          mcTimeUnits : Int = 1000) = {
    val movesInTimeUnit : Int = moves.map(_._2).sum
    val accumulatedMoveNumbers = moves.map(_._2).scanLeft(0)(_+_).tail
    val movesAccumulated = (moves.map(_._1), accumulatedMoveNumbers).zipped
    Stream.continually({
      Seq.fill(movesInTimeUnit)(Random.nextInt(movesInTimeUnit)).flatMap({
        r => {
          movesAccumulated.find({ m => r < m._2}) match {
            case Some(v) => Some(v._1)
            case _ => None
          }
        }
      })

    }
    //  (
        //Random.nextInt(structure.length),
      //  Random.shuffle(moves)
      //)
    ).zipWithIndex.take(mcTimeUnits).foldLeft(structure) {
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
