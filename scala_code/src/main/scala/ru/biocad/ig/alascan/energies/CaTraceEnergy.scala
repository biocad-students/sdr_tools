package ru.biocad.ig.alascan.energies

import ru.biocad.ig.common.structures.aminoacid.SimplifiedChain
import ru.biocad.ig.alascan.constants.energy_terms._
import ru.biocad.ig.common.algorithms.geometry.ManifoldUtils
import scala.io.Source
import spray.json._

import E14JsonProtocol._
import E14avgJsonProtocol._

class CaTraceEnergy() extends BasicEnergy {
  val e14 : E14 = JsonParser(Source.fromURL(getClass.getResource("/MCDP_json/r14aa12.json")).getLines().mkString("")).convertTo[E14]
  val e14avg : E14avg = JsonParser(Source.fromURL(getClass.getResource("/MCDP_json/r14avg12.json")).getLines().mkString("")).convertTo[E14avg]

  override def get(aminoacids : SimplifiedChain) : Double = {
    val r14Seq : Seq[Double] = (1 to aminoacids.size - 3).map({
      case i : Int =>
      {
        val b = (Seq(i, i + 1, i + 2), Seq(i - 1, i, i + 1)).zipped.map({
          case (x : Int, y : Int) => aminoacids(x).ca - aminoacids(y).ca
        })
        b.reduceLeft(_ + _).lengthSquared * math.signum(ManifoldUtils.getDeterminant(b.map(_.coordinates)))
      }})
    (r14Seq, 0.0 +: r14Seq, (1 to aminoacids.size - 3)).zipped.map({
      case (r14, r14_prev, i)  => 3*e14.get(r14, aminoacids(i).name, aminoacids(i + 1).name) + e14avg.get(r14, r14_prev)
    }).sum
  }
}
