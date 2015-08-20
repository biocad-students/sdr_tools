package ru.biocad.ig.alascan.constants.json

import spray.json._
import ru.biocad.ig.alascan.constants.{BackboneInfo}

/**
object AlascanConstantsJsonProtocol extends DefaultJsonProtocol {
  implicit val alascanConstantsFormat : JsonFormat[BackboneInfo] = lazyFormat(jsonFormat1(BackboneInfo))

  implicit object AlascanConstantsJsonFormat extends RootJsonFormat[BackboneInfo] {
    def write(info: BackboneInfo) = info.toJson

//    def read(value: JsValue) =  new BackboneInfo(value)
}
}
*/
