package ru.biocad.ig.alascan.energies

import ru.biocad.ig.common.structures.aminoacid.SimplifiedChain
import ru.biocad.ig.common.structures.geometry.Lattice
import ru.biocad.ig.alascan.constants.LatticeConstants
import ru.biocad.ig.alascan.constants.energy_terms._
import scala.io.Source
import spray.json._

import EOneJsonProtocol._

class BurialEnergy(val lattice : Lattice) extends BasicEnergy {
  val eone : EOne = Lattice.loadFromFile[EOne](lattice.latticeConstants.energyTermsParameters("eone"))

  //this is very-very SLOW implementation, should refactor
  override def get(chain : SimplifiedChain) : Double = {
    val contactMap = chain.contactMap
    (0 to chain.size - 1).map({
      i => {
        val numberOfContacts = contactMap(i).count(_ >= i+1)
        eone.get(chain(i).name, numberOfContacts)
      }
    }).sum
  }
}
