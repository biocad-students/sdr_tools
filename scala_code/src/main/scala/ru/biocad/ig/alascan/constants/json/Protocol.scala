package ru.biocad.ig.alascan.constants.json

import spray.json._
import ru.biocad.ig.alascan.constants.{BackboneInfo}


object AlascanConstantsJsonProtocol extends DefaultJsonProtocol {

  implicit object AlascanConstantsJsonFormat extends RootJsonFormat[BackboneInfo] {
    def write(info: BackboneInfo) = info.toJson

    def read(value: JsValue) = {
      value.asJsObject.getFields("data", "meshSize") match {
        case Seq(JsObject(data), JsNumber(meshSize)) => {
          new BackboneInfo(data.mapValues(_.convertTo[Map[String, JsValue]].map({
            case (k, v) => (k.toInt, v.convertTo[Map[String, JsValue]].map({
              case (k, v) => (k.toInt, v.convertTo[Map[String, Map[String, Seq[Double]]]].map({
            case (k,v) => (k.toInt, v)
          }))}))})), meshSize.toDouble)
          }
        case _ => throw new DeserializationException("BackboneInfo expected")
      }
    }
  }
}

import AlascanConstantsJsonProtocol._
