package ru.biocad.ig.alascan.energies

import ru.biocad.ig.common.structures.aminoacid.SimplifiedChain
import ru.biocad.ig.common.algorithms.geometry.HydrogenBondsFinder

import ru.biocad.ig.alascan.constants._

class HydrogenBondEnergy(latticeConstants : LatticeConstants,
      hBondCondition :  (SimplifiedChain, Int, Int) => Boolean) extends BasicEnergy {
  override def get(aminoacids : SimplifiedChain) : Double = {
    var E = 0.0
    val b = new HydrogenBondsFinder(hBondCondition, aminoacids)
    latticeConstants.eHH*b.cooperativeCount + latticeConstants.eH*b.bondsCount
    //TODO: fix this, add cooperativity
  }
}
