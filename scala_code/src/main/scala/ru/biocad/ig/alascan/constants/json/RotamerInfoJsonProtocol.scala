package ru.biocad.ig.alascan.constants.json

import spray.json._
import ru.biocad.ig.alascan.constants.RotamerInfo

object RotamerLibraryJsonProtocol extends DefaultJsonProtocol {
  implicit object RotamerLibraryJsonFormat extends RootJsonFormat[RotamerLibrary] {
    def write(info: RotamerLibrary) = info.toJson

    def read(value: JsValue) = {
      value.asJsObject.getFields("data", "meshSize") match {
        case Seq(JsObject(data), JsNumber(meshSize)) => {
          new RotamerLibrary(data.mapValues(_.convertTo[Map[String, JsValue]].map({
            case (k, v) => (k.toInt, v.convertTo[Map[String, JsValue]].map({
              case (k, v) => (k.toInt, v.convertTo[Map[String, Map[String, Seq[Double]]]].map({
            case (k,v) => (k.toInt, v)
          }))}))})), meshSize.toDouble)
          }
        case _ => throw new DeserializationException("Rotamer expected")
      }
    }
  }
}
