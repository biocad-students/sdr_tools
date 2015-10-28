package ru.biocad.ig.common.algorithms.geometry

import ru.biocad.ig.common.structures.aminoacid.SimplifiedAminoacid

/** Finds hydrogen bonds with the help of Greer & Levitt method
  */
case class HydrogenBondsFinder(HBondCondition : (Seq[SimplifiedAminoacid], Int, Int) => Boolean,
    structure : Seq[SimplifiedAminoacid]) {
  def findBondsForStructure(structure : Seq[SimplifiedAminoacid]) = {
    val hBonds = (0 to structure.size - 1).map({ i =>
      (0 to structure.size - 1).map({j =>
        HBondCondition(structure, i, j)
      })
    })
    val b = (0 to structure.size - 1).map({i =>
      (i + 1 to structure.size - 1).count({j=> hBonds(i)(j) })
    }).foldLeft(0)(_ + _)
    val c = (0 to structure.size - 1).map({i =>
      (i + 1 to structure.size - 1).count({j=>

          (j <= structure.size - 1 && i > 0 && hBonds(i)(j)) && (
            (hBonds(i + 1)(j + 1)) ||
            (hBonds(i - 1)(j + 1)))
        })
    }).foldLeft(0)(_ + _)

    //TODO: ...this is unfinished method, should return actual hydrogen bond network
    (b, c)
  }

  val (bondsCount, cooperativeCount) = findBondsForStructure(structure)//TODO: implement

}
