package ru.biocad.ig.alascan.energies

import ru.biocad.ig.common.structures.aminoacid.{SimplifiedChain, SimplifiedAminoacid}
import ru.biocad.ig.alascan.constants.energy_terms._
import ru.biocad.ig.common.structures.geometry.Lattice

import scala.io.Source
import spray.json._

import EPairJsonProtocol._


class PairEnergy(val lattice : Lattice) extends BasicEnergy {

  val epair : EPair = Lattice.loadFromFile[EPair](lattice.latticeConstants.energyTermsParameters("epair"))

  //TODO: rewrite later
  def get_E_two(i : Int, j : Int, ai : SimplifiedAminoacid, aj : SimplifiedAminoacid, f : Double) : Double = {
    val rRepulsive = lattice.rotamerRadiusInfo.getRrep(ai.name, aj.name)
    val rInteraction = lattice.rotamerRadiusInfo.getR(ai.name, aj.name)
    val pairEnergy = epair.get(ai.name, aj.name)
    (ai.rotamer - aj.rotamer).length match {
      case x if x < rRepulsive => lattice.rotamerRadiusInfo.eRepulsive
      case x if x < rInteraction && pairEnergy >= 0.0 && (j - i == 5 || j - i == 6) => pairEnergy * 0.6
      case x if x < rInteraction && pairEnergy >= 0.0 => pairEnergy
      case _ if j - i == 5 || j - i == 6 => pairEnergy * f * 0.6
      case _ => pairEnergy * f
    }
  }

  override def get(aminoacids : SimplifiedChain) : Double = {
    val constAngle20 = math.pow(math.cos(math.toRadians(20.0)), 2)
    (2 to aminoacids.size - 3).flatMap({
      i => (i + 4 to aminoacids.size - 3).map({
        j => {
          //TODO: check actual +- 2 for f
          val ui_uj = (aminoacids(i + 2).ca - aminoacids(i - 2).ca).normalize *
                      (aminoacids(j + 2).ca - aminoacids(j - 2).ca).normalize
          val f = 1.0 - math.pow(ui_uj * ui_uj - constAngle20, 2)
          get_E_two(i, j, aminoacids(i), aminoacids(j), f)
        }
      })
    }).sum
  }
}
