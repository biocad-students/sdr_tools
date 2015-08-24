package ru.biocad.ig.common.algorithms

import ru.biocad.ig.common.structures.geometry.LatticeBasicMove
import scala.util.Random
/**
object MonteCarloRunner{
  def run(structure, moves :: Seq[LatticeBasicMove], getEnergy, ck_rule, numberOfMoves = 1000) {
    Stream.continually(
      (
        Random.nextInt(structure.length),
        Random.nextInt(moves.length)
      )
    ).zipWithIndex.take(numberOfMoves).foldLeft(structure) {
      (currentStructure, ((position, move), time)) => {
        val newStructure = moves(move).modify(currentStructure, position)
        val oldE = getEnergy(currentStructure)
        val newE = getEnergy(newStructure)
        if (oldE < newE) {
          newStructure
        }
        else {
          currentStructure
        }
      }

    }
  }
}*/
