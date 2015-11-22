package ru.biocad.ig.alascan.energies

import ru.biocad.ig.common.structures.aminoacid.SimplifiedChain
import ru.biocad.ig.common.algorithms.geometry.HydrogenBondsFinder
import ru.biocad.ig.common.structures.geometry.Lattice

import ru.biocad.ig.alascan.constants._

class HydrogenBondEnergy(val lattice : Lattice) extends BasicEnergy {

  //this returns true if they can, false otherwise - quite simple
  def canFormHBond(aminoacids : SimplifiedChain, i : Int, j : Int) : Boolean = {
    val r_ij = aminoacids(j).ca - aminoacids(i).ca
    if (i == 0 || j == 0)
      return false
    val b_i_b_i_1 = aminoacids(i - 1).ca - aminoacids(i).ca //TODO: check if i == 0
    val b_j_b_j_1 = aminoacids(j - 1).ca - aminoacids(j).ca
    (i - j).abs >= 3 && lattice.latticeConstants.distanceConditionForHBonds(r_ij.length) &&
      (b_i_b_i_1*r_ij).abs <= lattice.latticeConstants.hBondAmax &&
      (b_j_b_j_1*r_ij).abs <= lattice.latticeConstants.hBondAmax
  }

  override def get(aminoacids : SimplifiedChain) : Double = {
    var E = 0.0
    val b = new HydrogenBondsFinder(canFormHBond, aminoacids)
    lattice.latticeConstants.eHH*b.cooperativeCount + lattice.latticeConstants.eH*b.bondsCount
    //TODO: fix this, add cooperativity
  }
}
