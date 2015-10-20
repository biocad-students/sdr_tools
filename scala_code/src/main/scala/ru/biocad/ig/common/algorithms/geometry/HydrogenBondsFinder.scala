package ru.biocad.ig.common.algorithms.geometry

import ru.biocad.ig.common.structures.aminoacid.SimplifiedAminoAcid

/** Finds hydrogen bonds with the help of Greer & Levitt method
  */
case class HydrogenBondsFinder(HBondCondition : (Seq[SimplifiedAminoAcid], Int, Int) => Boolean,
    structure : Seq[SimplifiedAminoAcid]) {
  def findBondsForStructure(structure : Seq[SimplifiedAminoAcid]) = {
    (1 to structure.size - 1).map({ i =>
      (i + 3 to structure.size - 1).filter({j =>
        HBondCondition(structure, i, j)
      })
    }).flatten
    //TODO: ...this is unfinished method, should return actual hydrogen bond network
  }

  val cooperativeCount = 0 //TODO:implement
  val bondsCount = 0//TODO: implement

}
