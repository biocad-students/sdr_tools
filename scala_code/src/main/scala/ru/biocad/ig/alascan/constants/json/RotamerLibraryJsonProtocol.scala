package ru.biocad.ig.alascan.constants.json

import spray.json._
import ru.biocad.ig.alascan.constants.{AminoacidLibrary, RotamerInfo}
import RotamerInfoJsonProtocol._

object RotamerLibraryJsonProtocol extends DefaultJsonProtocol {
  implicit object RotamerLibraryJsonFormat extends RootJsonFormat[AminoacidLibrary[RotamerInfo]] {
    def write(info: AminoacidLibrary[RotamerInfo]) = info.toJson

    def read(value: JsValue) = {
      value.asJsObject.getFields("data", "meshSize") match {
        case Seq(JsObject(data), JsNumber(meshSize)) => {
          new AminoacidLibrary[RotamerInfo](data.mapValues(_.convertTo[Map[String, JsValue]].map({
            case (k, v) => (k.toInt, v.convertTo[Map[String, JsValue]].map({
              case (k, v) => (k.toInt, v.convertTo[Map[String, JsValue]].map({
            case (k,v) => (k.toInt, v.convertTo[RotamerInfo])
          }))}))})), meshSize.toDouble)
          }
        case _ => throw new DeserializationException("Rotamer expected")
      }
    }
  }
}
import RotamerLibraryJsonProtocol._
