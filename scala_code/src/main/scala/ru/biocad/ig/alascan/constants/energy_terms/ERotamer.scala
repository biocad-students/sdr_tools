package ru.biocad.ig.alascan.constants.energy_terms

import spray.json._
import DefaultJsonProtocol._
import ru.biocad.ig.common.structures.aminoacid.SimplifiedAminoAcid

case class ERotamer(
      val phi : Map[Int, Array[Double]],
      val energies : Map[String, Map[Int, Array[Double]]]) {

  def get(aa1 : SimplifiedAminoAcid, aa2 : SimplifiedAminoAcid, distance : Int) : Double = {
    1.0
  }
}

object ERotamerJsonProtocol extends DefaultJsonProtocol {
  implicit val ERotamerFormat = jsonFormat2(ERotamer)

  implicit object ERotamerLibraryJsonFormat extends RootJsonFormat[ERotamer] {
    def write(info: ERotamer) = info.toJson

    def read(value: JsValue) = {
      value.asJsObject.getFields("phi", "energies") match {
        case Seq(JsObject(phi), JsObject(energies)) => {
          new ERotamer(
            phi.map({case (k, v) => (k.toInt, v.convertTo[Array[Double]])}),
            energies.mapValues(_.convertTo[Map[String, Array[Double]]].map(
              {case (k, v) => (k.toInt, v)}))
          )
        }
        case _ => throw new DeserializationException("ERotamer expected")
      }
    }
  }
}

import ERotamerJsonProtocol._
