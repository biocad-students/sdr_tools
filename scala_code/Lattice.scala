package ru.biocad.ig.common.structures.geometry

/**coarse lattice class. currently implemented as a singleton object with a lot of constant values.
Later should refactor to class with file parameters loading, thus making creation of different lattice objects possible
*/
object LatticeConstants {
  val MESH_SIZE = 1.7
  val CA_ANGLE_CONSTRAINTS = {"min": 78.5, "max": 143.1}
  val CA_MIN_DISTANCE = 0.0 // unspecified

  //H-bond energies
  val E_H = 0.5
  val E_HH = 0.75
  val H_bond_R_min = 4.8
  val H_bond_R_max = 7.0
  def H_bond_distance_condition(r: Double): Boolean = r >= H_bond_R_min&& r <= H_bond_R_max
}

object Lattice {
  /** helper methods*/
  //this returns true if they can, false otherwise - quite simple
  def can_form_H_bond(aminoacids: Seq[SimplifiedAminoAcid], i : Int, j : Int) : Boolean = {
    val r_ij = aminoacids(j).ca - aminoacids(i).ca
    i - j >= 3 && LatticeConstants.H_bond_distance_condition(r_ij.length) 
    //LatticeConstants
  }
  /** energy methods*/
  def get_E_CA_trace(aminoacids : Seq[SimplifiedAminoAcid]) : Double = {

  }
  def get_E_H_bond(aminoacids : Seq[SimplifiedAminoAcid]) : Double = {
    can_form_H_bond(aminoacids, i, j)

  }
  def get_E_rot(aminoacids : Seq[SimplifiedAminoAcid]): Double = {

  }
  def get_E_SG_local(aminoacids : Seq[SimplifiedAminoAcid]) : Double = {

  }
  def get_E_one(aminoacids : Seq[SimplifiedAminoAcid]) : Double = {

  }
  def get_E_pair(aminoacids : Seq[SimplifiedAminoAcid]) : Double = {

  }
  def get_E_tem(aminoacids : Seq[SimplifiedAminoAcid]) : Double = {

  }

  //TODO: we have pair of chains, that means we somehow should utilize that when we compute total energy
  def get_E(aminoacids : Seq[SimplifiedAminoAcid]) : Double = {
    get_E_CA_trace(aminoacids) +
    get_E_H_bond(aminoacids) +
    get_E_rot(aminoacids) +
    get_E_SG_local(aminoacids) +
    get_E_one(aminoacids) +
    get_E_pair(aminoacids) +
    get_E_tem(aminoacids)
  }

}
