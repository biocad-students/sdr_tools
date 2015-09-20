package ru.biocad.ig.alascan.constants

/**coarse lattice class. currently implemented as a singleton object with a lot of constant values.
Later should refactor to class with file parameters loading, thus making creation of different lattice objects possible
*/
object LatticeConstants {
  val MESH_SIZE = 1.22

  object Borders extends Enumeration() {
    val Min, Max = Value
  }
  import Borders._

  val CA_ANGLE_CONSTRAINTS = Map(Min -> 78.5, Max -> 143.1)
  val CA_MIN_DISTANCE = 0.0 // unspecified

  //H-bond energies
  val E_H = 0.5
  val E_HH = 0.75
  val H_bond_R_min = 4.8
  val H_bond_R_max = 7.0
  val H_bond_a_max = 17.3
  def H_bond_distance_condition(r: Double): Boolean = r >= H_bond_R_min&& r <= H_bond_R_max
  //rotamers
  //val ROTAMERS = Map()
  //TODO: implement as a map number of rotamers, maybe with ProteinAlphabet class from ig-toolkit
  //contact
  val CONTACT_CUTOFF = 4.2
}
