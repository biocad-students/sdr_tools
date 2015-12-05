package ru.biocad.ig.alascan.energies

import ru.biocad.ig.common.structures.aminoacid.{SimplifiedChain, SimplifiedAminoacid}
import ru.biocad.ig.common.structures.geometry.{Lattice, GeometryVector, Vector3d}
import ru.biocad.ig.alascan.constants.energy_terms._
import scala.io.Source
import spray.json._

import EOneGlobularJsonProtocol._

/** This is optional energy term, it can be turned on/off via `config/lattice_params.json`.
  * Should be used for globular proteins with one domain and chain.
  * Uses `parameters` key in `config/lattice_params.json` named `eoneGlobular`.
  * Implementation is based on articles [folding107,folding116]
  */
class OneBodyEnergy(val lattice : Lattice) extends BasicEnergy {
  val eoneGlobular : EOneGlobular = lattice.loadFromFile[EOneGlobular](lattice.latticeConstants.energyTermsParameters("eoneGlobular"))

  //this is very-very SLOW implementation, should refactor
  override def get(chain : SimplifiedChain) : Double = {
    val contactMap = chain.contactMap
    val gyrationRadius = 2.2*math.exp(0.38*math.log(chain.size))//estimated radius of gyration
    val centerOfMasses : GeometryVector = chain.foldLeft(Vector3d(0, 0, 0) : GeometryVector)({
        case (result, aa) => result + aa.ca
    }) / chain.size
    chain.map({
      case aa => eoneGlobular.get(aa.name, (aa.ca - centerOfMasses).length, gyrationRadius)
    }).sum
  }
}
