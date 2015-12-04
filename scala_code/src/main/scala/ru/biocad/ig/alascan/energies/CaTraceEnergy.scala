package ru.biocad.ig.alascan.energies

import ru.biocad.ig.common.structures.aminoacid.SimplifiedChain
import ru.biocad.ig.alascan.constants.energy_terms._
import ru.biocad.ig.common.algorithms.geometry.ManifoldUtils
import ru.biocad.ig.common.structures.geometry.Lattice

import scala.io.Source
import spray.json._

import E14JsonProtocol._
import E14avgJsonProtocol._

class CaTraceEnergy(val lattice : Lattice) extends BasicEnergy {
  val e14 : E14 = Lattice.loadFromFile[E14](lattice.latticeConstants.energyTermsParameters("e14"))
  val e14avg : E14avg = Lattice.loadFromFile[E14avg](lattice.latticeConstants.energyTermsParameters("e14avg"))

  override def get(chain : SimplifiedChain) : Double = {
    val r14Seq = chain.vectors.sliding(3, 1).map({
      case x => x.reduceLeft(_ + _).lengthSquared * math.signum(ManifoldUtils.getDeterminant(x.map(_.coordinates)))
    }).toList
    3*e14.get(r14Seq.head, chain(0).name, chain(1).name) +
      (r14Seq.tail, r14Seq, Stream from 1).zipped.map({
        case (r14, r14_prev, i)  =>
          3*e14.get(r14, chain(i).name, chain(i + 1).name) + e14avg.get(r14, r14_prev)
      }).sum
  }
}
