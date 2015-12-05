package ru.biocad.ig.alascan.energies

import ru.biocad.ig.common.structures.aminoacid.SimplifiedChain
import ru.biocad.ig.alascan.constants.energy_terms._
import ru.biocad.ig.common.structures.geometry.Lattice

import scala.io.Source
import spray.json._

import EPairJsonProtocol._


/** This is optional energy term, it can be turned on/off via `config/lattice_params.json`.
  * Uses `parameters` key in `config/lattice_params.json` named `epair`.
  * Implementation is based on articles [folding116,folding142].
  */
class TemplateEnergy(val lattice : Lattice) extends BasicEnergy {
  val epair : EPair = lattice.loadFromFile[EPair](lattice.latticeConstants.energyTermsParameters("epair"))

  override def get(chain : SimplifiedChain) : Double = {
    val contactMap = chain.contactMap
    //TODO: check borders
    (4 to chain.size - 5).flatMap({
      i => (i + 4 to chain.size - 5).map({
        j => {
          Seq(-4,-3,3,4).map({k => {
            (if (contactMap(i).contains(j) && contactMap(i + k).contains(j + k))
              epair.apab(chain(i).name)(chain(j).name) +
              epair.apab(chain(i + k).name)(chain(j + k).name)
            else 0.0)+
            (if (contactMap(i).contains(j) && contactMap(i + k).contains(j - k))
              epair.apab(chain(i).name)(chain(j).name) +
              epair.apab(chain(i + k).name)(chain(j - k).name)
            else 0.0)}}).sum
        }
      })
    }).sum
    //???
  }
}
