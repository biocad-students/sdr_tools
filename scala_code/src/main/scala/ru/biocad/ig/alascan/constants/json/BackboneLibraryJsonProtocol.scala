package ru.biocad.ig.alascan.constants.json

import spray.json._
import ru.biocad.ig.alascan.constants.{AminoacidLibrary, BackboneInfo}

import BackboneInfoJsonProtocol._

object BackboneLibraryJsonProtocol extends DefaultJsonProtocol {

  implicit object BackboneLibraryJsonFormat extends RootJsonFormat[AminoacidLibrary[BackboneInfo]] {
    def write(info: AminoacidLibrary[BackboneInfo]) = info.toJson

    def read(value: JsValue) = {
      value.asJsObject.getFields("data", "meshSize") match {
        case Seq(JsObject(data), JsNumber(meshSize)) => {
          new AminoacidLibrary[BackboneInfo](data.mapValues(_.convertTo[Map[String, JsValue]].map({
            case (k, v) => (k.toInt, v.convertTo[Map[String, JsValue]].map({
              case (k, v) => (k.toInt, v.convertTo[Map[String, JsObject]].map({
            case (k,v) => (k.toInt, v.convertTo[BackboneInfo])
          }))}))})), meshSize.toDouble)
          }
        case _ => throw new DeserializationException("BackboneInfo expected")
      }
    }
  }
}

import BackboneLibraryJsonProtocol._
