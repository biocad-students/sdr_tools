package ru.biocad.ig.alascan.constants.json

import spray.json._
import ru.biocad.ig.alascan.constants.SidechainInfo

import ru.biocad.ig.common.structures.aminoacid.RotamerInfo
import RotamerJsonProtocol._

object SidechainInfoJsonProtocol extends DefaultJsonProtocol {
  implicit object SidechainInfoFormat extends RootJsonFormat[SidechainInfo] {
    def write(info: SidechainInfo) = info.toJson

    def read(value: JsValue) = {
      println(value)
      value.asJsObject.getFields("representatives", "amounts", "total") match {
        case Seq(JsArray(representatives), JsArray(amounts), JsNumber(total)) => {
          new SidechainInfo(representatives.map(_.convertTo[RotamerInfo]), amounts.map(_.convertTo[Int]), total.toInt)
          }
        case _ => throw new DeserializationException("SidechainInfo expected")
      }
    }
  }
}
import SidechainInfoJsonProtocol._
