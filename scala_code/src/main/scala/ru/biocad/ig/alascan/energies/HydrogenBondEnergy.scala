package ru.biocad.ig.alascan.energies

import ru.biocad.ig.common.structures.aminoacid.SimplifiedChain
import ru.biocad.ig.common.algorithms.geometry.HydrogenBondsFinder
import ru.biocad.ig.common.structures.geometry.Lattice

import ru.biocad.ig.alascan.constants._


/** This is optional energy term, it can be turned on/off via `config/lattice_params.json`.
  * Uses `parameters` key in `config/lattice_params.json` named `eoneGlobular`.
  * Implementation is based on articles [folding116,folding142].
  * However, there is no checks if one of forming atoms is proline, thus there are no explicit evidence that proline can form maximum 1 bond.
  * In fact, it is not obvious that any other amino acid can form <= 2 bonds, but I believe it follows from geometric condition implemented in `canFormHBond` method.
  * Other possible realization is based on positions of residues' united atoms is given in [folding163], it is more clear, but I decided to use this because currently implemented simplified chain includes C_\alpha atoms.
  */
class HydrogenBondEnergy(val lattice : Lattice) extends BasicEnergy {

  //this returns true if they can, false otherwise - quite simple
  def canFormHBond(chain : SimplifiedChain, i : Int, j : Int) : Boolean = {
    val r_ij = chain(j).ca - chain(i).ca
    if (i == 0 || j == 0 || i >= chain.vectors.size || j >= chain.vectors.size)
      return false
    val b_i_b_i_1 = chain.vectors(i - 1) - chain.vectors(i)
    val b_j_b_j_1 = chain.vectors(j - 1) - chain.vectors(j)
    (i - j).abs >= 3 && lattice.latticeConstants.distanceConditionForHBonds(r_ij.length) &&
      (b_i_b_i_1*r_ij).abs <= lattice.latticeConstants.hBondAmax &&
      (b_j_b_j_1*r_ij).abs <= lattice.latticeConstants.hBondAmax
  }

  override def get(aminoacids : SimplifiedChain) : Double = {
    val b = new HydrogenBondsFinder(canFormHBond, aminoacids)
    lattice.latticeConstants.eHH * b.cooperativeCount + lattice.latticeConstants.eH * b.bondsCount
  }
}
