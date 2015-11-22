package ru.biocad.ig.alascan.constants.energy_terms

import spray.json._
import DefaultJsonProtocol._

case class EOneGlobular(val data : Map[String, Array[Double]]) {

    def get(aminoacidName : String,
            distance : Double,
            gyrationRadius : Double) : Double = data.get(aminoacidName) match {
        case Some(aaData) => {
          (2 to 18).find(
            i => distance >= i * gyrationRadius * 0.1 &&
                 distance < (i + 1) * gyrationRadius * 0.1) match {
                   case Some(i) => aaData(i - 2)
                   case None => 0.0
                 }
        }
        case None => 0.0
    }
}


object EOneGlobularJsonProtocol extends DefaultJsonProtocol {
  implicit object EOneGlobularJsonFormat extends RootJsonFormat[EOneGlobular] {
    def write(info: EOneGlobular) = info.data.toJson

    def read(value: JsValue) = new EOneGlobular(value.asJsObject.convertTo[Map[String, Array[Double]]])
  }
}

import EOneGlobularJsonProtocol._
