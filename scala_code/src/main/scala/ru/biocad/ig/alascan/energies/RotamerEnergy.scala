package ru.biocad.ig.alascan.energies

import ru.biocad.ig.common.structures.aminoacid.SimplifiedChain
import ru.biocad.ig.alascan.constants.energy_terms._
import ru.biocad.ig.common.algorithms.geometry.ManifoldUtils
import scala.io.Source
import spray.json._

import ERotamerJsonProtocol._

class RotamerEnergy() extends BasicEnergy {
  val eRotamer : ERotamer = JsonParser(Source.fromURL(getClass.getResource("/MCDP_json/rotamer_energies.json")).getLines().mkString("")).convertTo[ERotamer]

  override def get(aminoacids : SimplifiedChain) : Double = {
    (2 to aminoacids.size - 2).map({
      case i => eRotamer.get(aminoacids(i), aminoacids(i - 1), aminoacids(i + 1))
    }).sum
  }
}
