package ru.biocad.ig.alascan.energies

import ru.biocad.ig.common.structures.aminoacid.SimplifiedChain
import ru.biocad.ig.alascan.constants.energy_terms._
import ru.biocad.ig.common.structures.geometry.Lattice

import scala.io.Source
import spray.json._

import ESgLocalJsonProtocol._

/** This is optional energy term, it can be turned on/off via `config/lattice_params.json`.
  * Uses `parameters` key in `config/lattice_params.json` named `eSglocal`.
  * Implementation is based on articles [folding116,folding142].
  */
class SGLocalEnergy(val lattice : Lattice) extends BasicEnergy {
  val eSglocal : ESgLocal = lattice.loadFromFile[ESgLocal](lattice.latticeConstants.energyTermsParameters("eSglocal"))

  override def get(aminoacids : SimplifiedChain) : Double = {
    (1 to aminoacids.size - 2).flatMap({
      i => (1 to 4).map({ k => if (i + k < aminoacids.size)
        eSglocal.get(aminoacids(i), aminoacids(i + k), k) else 0.0
        })
    }).sum
  }
}
