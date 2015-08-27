package ru.biocad.ig.alascan.constants.json

import spray.json._
import ru.biocad.ig.common.structures.geometry.{GeometryVector, Vector}

object GeometryVectorJsonProtocol extends DefaultJsonProtocol {
  //implicit val geometryVectorFormat : JsonFormat[GeometryVector] = jsonFormat1(GeometryVector)

  implicit object GeometryVectorJsonFormat extends JsonFormat[GeometryVector] {
    def write(info: GeometryVector) = info.toJson

    def read(value: JsValue) : GeometryVector = {
      value.asJsObject.getFields("coordinates") match {
        case Seq(JsArray(coordinates)) => new Vector(coordinates.map(_.convertTo[Double]))
        case _ => throw new DeserializationException("BackboneInfo expected")
      }
        //
    }
  }
}

import GeometryVectorJsonProtocol._
