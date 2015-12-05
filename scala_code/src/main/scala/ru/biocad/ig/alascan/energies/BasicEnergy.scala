package ru.biocad.ig.alascan.energies

import ru.biocad.ig.common.structures.aminoacid.SimplifiedChain

/** basic trait for energy terms.
  * Each term should also have constructor with 1 parameter of type [[ru.biocad.ig.common.structures.geometry.Lattice]]
  * For each of terms, method `get` is called each time energy of chain is computed.
  * How to use your custom energy term: in main settings file (by default `config/lattice_params.json`) add
  * full class path for newly created class and its weight to hash named `energyTerms`.
  * If class should load some json with parameters, add its path to hash named `parameters`, with custom key,
  * and load in your class like this:
  * {{{
  *     class RotamerEnergy(val lattice : Lattice) extends BasicEnergy {
  *        val eRotamer : ERotamer = lattice.loadFromFile[ERotamer](lattice.latticeConstants.energyTermsParameters("eRotamer"))
  *        // next follows implementation of `get` method
  *        // and other code
  *     }
  * }}}
  */
trait BasicEnergy {
  def get(aminoacids : SimplifiedChain) : Double = ???
}
