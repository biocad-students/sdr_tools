package ru.biocad.ig.alascan.energies

import ru.biocad.ig.common.structures.aminoacid.SimplifiedChain
import ru.biocad.ig.alascan.constants.energy_terms._
import ru.biocad.ig.common.algorithms.geometry.ManifoldUtils
import ru.biocad.ig.common.structures.geometry.Lattice

import scala.io.Source
import spray.json._

import ERotamerJsonProtocol._

class RotamerEnergy(val lattice : Lattice) extends BasicEnergy {
  val eRotamer : ERotamer = lattice.loadFromFile[ERotamer]("/MCDP_json/rotamer_energies.json")

  override def get(aminoacids : SimplifiedChain) : Double = {
    (2 to aminoacids.size - 2).map({
      case i => eRotamer.get(aminoacids(i), aminoacids(i - 1), aminoacids(i + 1))
    }).sum
  }
}
