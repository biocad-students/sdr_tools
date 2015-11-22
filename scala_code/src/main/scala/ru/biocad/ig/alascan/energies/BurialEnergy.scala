package ru.biocad.ig.alascan.energies

import ru.biocad.ig.common.structures.aminoacid.SimplifiedChain
import ru.biocad.ig.common.structures.geometry.Lattice
import ru.biocad.ig.alascan.constants.energy_terms._
import scala.io.Source
import spray.json._

import EOneJsonProtocol._

class BurialEnergy(val lattice : Lattice) extends BasicEnergy {
  val eone : EOne = JsonParser(Source.fromURL(getClass.getResource("/MCDP_json/EONE.json")).getLines().mkString("")).convertTo[EOne]

  //this is very-very SLOW implementation, should refactor
  override def get(chain : SimplifiedChain) : Double = {
    val contactMap = lattice.buildContactMap(chain)
    (0 to chain.size - 1).map({
      i => {
        val numberOfContacts = (i + 1 to chain.size - 1).count({ case j => contactMap(i)(j) })
        eone.get(chain(i).name, numberOfContacts)
      }
    }).sum
  }
}
