package ru.biocad.ig.alascan.constants.json

import spray.json._

import ru.biocad.ig.common.structures.aminoacid.Rotamer
import GeometryVectorJsonProtocol._
import ru.biocad.ig.common.structures.geometry.{GeometryVector, Vector, Vector3d}

object RotamerJsonProtocol extends DefaultJsonProtocol {

  implicit object RotamerJsonFormat extends RootJsonFormat[Rotamer] {
    def write(r: Rotamer) = r.toJson

    def read(value: JsValue) = {
      value.asJsObject.getFields("atoms", "center") match {
        case Seq(JsObject(atoms), JsObject(center)) =>{
          println(center)
          new Rotamer(atoms.mapValues(_.convertTo[GeometryVector]),
            Vector3d(0,0,0))
          }
        case _ => throw new DeserializationException("Rotamer expected")
      }
    }
  }
}

import RotamerJsonProtocol._
