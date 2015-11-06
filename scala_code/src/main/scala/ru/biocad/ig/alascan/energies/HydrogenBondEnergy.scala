package ru.biocad.ig.alascan.energies

import ru.biocad.ig.common.structures.aminoacid.SimplifiedChain
import ru.biocad.ig.common.algorithms.geometry.HydrogenBondsFinder

import ru.biocad.ig.alascan.constants._

class HydrogenBondEnergy(hBondCondition :  (SimplifiedChain, Int, Int) => Boolean) extends BasicEnergy {
  override def get(aminoacids : SimplifiedChain) : Double = {
    var E = 0.0
    val b = new HydrogenBondsFinder(hBondCondition, aminoacids)
    LatticeConstants.E_HH*b.cooperativeCount + LatticeConstants.E_H*b.bondsCount
    //TODO: fix this, add cooperativity
  }
}
