package ru.biocad.ig.alascan.constants.energy_terms

import spray.json._
import DefaultJsonProtocol._
import ru.biocad.ig.common.structures.aminoacid.SimplifiedAminoacid
import ru.biocad.ig.common.structures.geometry.GeometryVector

case class ERotamer(
      val phi : Map[Int, Seq[Double]],
      val energies : Map[String, Map[Int, Seq[Double]]]) {

  def get(current : SimplifiedAminoacid, prev : SimplifiedAminoacid, next : SimplifiedAminoacid) : Double = {
    val v1 : GeometryVector = prev.ca - current.ca
    val v2 : GeometryVector = next.ca - current.ca
    val distance : Int = (prev.ca - next.ca).lengthSquared.toInt
    println(distance)
    if (phi.contains(distance)) {
      val angle = Math.round(Math.toDegrees(Math.acos((v1*v2) /(v1.length*v2.length))))
      phi(distance).foreach(println)
      println(angle)
      val index = phi(distance).indexWhere({case x => Math.abs(x - angle) < 1.0}) // TODO: add epsilon or some sort of approx
      if (index >= 0){
        val values = energies.getOrElse(current.name, Map[Int, Seq[Double]]()).getOrElse(distance, Seq[Double]())
        if (index < values.size)
          return values(index)
      }
        //
    }
    0.0
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
            phi.map({case (k, v) => (k.toInt, v.convertTo[Seq[Double]])}),
            energies.mapValues(_.convertTo[Map[String, Seq[Double]]].map(
              {case (k, v) => (k.toInt, v)}))
          )
        }
        case _ => throw new DeserializationException("ERotamer expected")
      }
    }
  }
}

import ERotamerJsonProtocol._
