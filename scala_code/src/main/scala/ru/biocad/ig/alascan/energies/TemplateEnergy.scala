package ru.biocad.ig.alascan.energies

import ru.biocad.ig.common.structures.aminoacid.SimplifiedChain
import ru.biocad.ig.alascan.constants.energy_terms._
import ru.biocad.ig.common.structures.geometry.Lattice

import scala.io.Source
import spray.json._

import EPairJsonProtocol._

class TemplateEnergy(val lattice : Lattice) extends BasicEnergy {
  val epair : EPair = JsonParser(Source.fromURL(getClass.getResource("/MCDP_json/PMFHIX_SCALE.json")).getLines().mkString("")).convertTo[EPair]

  override def get(chain : SimplifiedChain) : Double = {
    val contactMap = lattice.buildContactMap(chain)
    //TODO: check borders
    (4 to chain.size - 5).flatMap({
      i => (i + 4 to chain.size - 5).map({
        j => {
          Seq(-4,-3,3,4).map({k => {
            (if (contactMap(i)(j) && contactMap(i + k)(j + k))
              epair.apab(chain(i).name)(chain(j).name) +
              epair.apab(chain(i + k).name)(chain(j + k).name)
            else 0.0)+
            (if (contactMap(i)(j) && contactMap(i + k)(j - k))
              epair.apab(chain(i).name)(chain(j).name) +
              epair.apab(chain(i + k).name)(chain(j - k).name)
            else 0.0)}}).sum
        }
      })
    }).sum
    //???
  }
}
