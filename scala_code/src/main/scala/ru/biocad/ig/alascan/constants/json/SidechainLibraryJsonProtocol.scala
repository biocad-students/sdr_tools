package ru.biocad.ig.alascan.constants.json

import spray.json._
import ru.biocad.ig.alascan.constants.{AminoacidLibrary, SidechainInfo}
import SidechainInfoJsonProtocol._

object SidechainLibraryJsonProtocol extends DefaultJsonProtocol {
  implicit object SidechainLibraryJsonFormat extends RootJsonFormat[AminoacidLibrary[SidechainInfo]] {
    def write(info: AminoacidLibrary[SidechainInfo]) = info.toJson

    def read(value: JsValue) = {
      value.asJsObject.getFields("data", "meshSize") match {
        case Seq(JsObject(data), JsNumber(meshSize)) => {
          new AminoacidLibrary[SidechainInfo](data.mapValues(_.convertTo[Map[String, JsValue]].map({
            case (k, v) => (k.toInt, v.convertTo[Map[String, JsValue]].map({
              case (k, v) => (k.toInt, v.convertTo[Map[String, JsValue]].map({
            case (k,v) => (k.toInt, v.convertTo[SidechainInfo])
          }))}))})), meshSize.toDouble)
          }
        case _ => throw new DeserializationException("SidechainLibrary expected")
      }
    }
  }
}
import SidechainLibraryJsonProtocol._
