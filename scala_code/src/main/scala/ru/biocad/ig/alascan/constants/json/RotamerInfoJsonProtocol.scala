package ru.biocad.ig.alascan.constants.json

import spray.json._
import ru.biocad.ig.alascan.constants.RotamerInfo

import ru.biocad.ig.common.structures.aminoacid.Rotamer
import RotamerJsonProtocol._

object RotamerInfoJsonProtocol extends DefaultJsonProtocol {
  implicit object RotamerInfoFormat extends RootJsonFormat[RotamerInfo] {
    def write(info: RotamerInfo) = info.toJson

    def read(value: JsValue) = {
      println(value)
      value.asJsObject.getFields("representatives", "amounts", "total") match {
        case Seq(JsArray(representatives), JsArray(amounts), JsNumber(total)) => {
          new RotamerInfo(representatives.map(_.convertTo[Rotamer]), amounts.map(_.convertTo[Int]), total.toInt)
          }
        case _ => throw new DeserializationException("RotamerInfo expected")
      }
    }
  }
}
import RotamerInfoJsonProtocol._
