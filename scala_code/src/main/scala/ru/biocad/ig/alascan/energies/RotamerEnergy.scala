package ru.biocad.ig.alascan.energies

import ru.biocad.ig.common.structures.aminoacid.SimplifiedChain
import ru.biocad.ig.alascan.constants.energy_terms._
import ru.biocad.ig.common.algorithms.geometry.ManifoldUtils
import ru.biocad.ig.common.structures.geometry.Lattice

import scala.io.Source
import spray.json._

import ERotamerJsonProtocol._

/** This is optional energy term, it can be turned on/off via `config/lattice_params.json`.
  * Uses `parameters` key in `config/lattice_params.json` named `eRotamer`.
  * Implementation is based on articles [folding116,folding142].
  */
class RotamerEnergy(val lattice : Lattice) extends BasicEnergy {
  val eRotamer : ERotamer = lattice.loadFromFile[ERotamer](lattice.latticeConstants.energyTermsParameters("eRotamer"))

  override def get(chain : SimplifiedChain) : Double = {
    chain.structure.sliding(3, 1).map({
      case Array(a1, a2, a3) => eRotamer.get(a2, a1, a3)
    }).sum
  }
}
