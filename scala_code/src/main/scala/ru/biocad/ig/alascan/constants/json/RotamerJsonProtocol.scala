package ru.biocad.ig.alascan.constants.json

import spray.json._

//import ru.biocad.ig.common.structures.aminoacid.Rotamer
import ru.biocad.ig.common.structures.aminoacid.RotamerInfo
import GeometryVectorJsonProtocol._
import ru.biocad.ig.common.structures.geometry.{GeometryVector, Vector, Vector3d}

object RotamerJsonProtocol extends DefaultJsonProtocol {

  implicit object RotamerJsonFormat extends RootJsonFormat[RotamerInfo] {
    def write(r: RotamerInfo) = r.toJson

    def read(value: JsValue) = {
      value.asJsObject.getFields("atoms", "center") match {
        case Seq(JsObject(atoms), JsObject(center)) => {
          new RotamerInfo(atoms.mapValues(_.convertTo[GeometryVector]),
            Vector3d(0,0,0))
          }
        case _ => throw new DeserializationException("RotamerInfo expected")
      }
    }
  }
}

import RotamerJsonProtocol._
