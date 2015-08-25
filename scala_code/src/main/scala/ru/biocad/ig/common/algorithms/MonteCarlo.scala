package ru.biocad.ig.common.algorithms

import ru.biocad.ig.common.structures.geometry.LatticeBasicMove
import ru.biocad.ig.common.structures.aminoacid.SimplifiedAminoAcid
import scala.util.Random

object MonteCarloRunner{
  def run(structure : Seq[SimplifiedAminoAcid],
          moves : Seq[LatticeBasicMove],
          getEnergy : Seq[SimplifiedAminoAcid] => Double,
          ck_rule : Int => Double,
          numberOfMoves : Int = 1000) {

    Stream.continually(
      (
        Random.nextInt(structure.length),
        Random.nextInt(moves.length)
      )
    ).zipWithIndex.take(numberOfMoves).foldLeft(structure) {
      case (currentStructure, ((position, move), time)) => {
        val newStructure = moves(move).move(currentStructure, position)
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
}
