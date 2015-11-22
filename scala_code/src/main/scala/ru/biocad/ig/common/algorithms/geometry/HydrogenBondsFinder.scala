package ru.biocad.ig.common.algorithms.geometry

import ru.biocad.ig.common.structures.aminoacid.{SimplifiedAminoacid, SimplifiedChain}

/** Finds hydrogen bonds with the help of Greer & Levitt method
  */
case class HydrogenBondsFinder(HBondCondition : (SimplifiedChain, Int, Int) => Boolean,
    structure : SimplifiedChain) {
  def findBondsForStructure(structure : SimplifiedChain) = {
    val hBonds = (0 to structure.size - 1).map({ i =>
      (0 to structure.size - 1).map(HBondCondition(structure, i, _))
    })
    val b = (0 to structure.size - 1).map({i =>
      (i + 1 to structure.size - 1).count(hBonds(i)(_))
    }).foldLeft(0)(_ + _)
    val c = (0 to structure.size - 1).map({i =>
      (i + 1 to structure.size - 1).count({j=>
          (j < structure.size - 1 && i > 0 && i < structure.size - 1 && hBonds(i)(j)) && (
            (hBonds(i + 1)(j + 1)) ||
            (hBonds(i - 1)(j + 1)))
        })
    }).foldLeft(0)(_ + _)

    //TODO: ...this is unfinished method, should return actual hydrogen bond network
    (b, c)
  }

  val (bondsCount, cooperativeCount) = findBondsForStructure(structure)//TODO: implement

}
