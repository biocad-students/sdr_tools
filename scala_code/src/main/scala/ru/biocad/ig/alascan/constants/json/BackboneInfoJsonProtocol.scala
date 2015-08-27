package ru.biocad.ig.alascan.constants.json

import spray.json._
import ru.biocad.ig.alascan.constants.BackboneInfo

import ru.biocad.ig.common.structures.geometry.GeometryVector

import GeometryVectorJsonProtocol._


object BackboneInfoJsonProtocol extends DefaultJsonProtocol {
  implicit object BackboneInfoJsonFormat extends RootJsonFormat[BackboneInfo] {
    def write(info: BackboneInfo) = info.toJson

    def read(value: JsValue) = {
          new BackboneInfo(value.convertTo[Map[String, JsValue]].mapValues(_.convertTo[GeometryVector]))
        //case _ => throw new DeserializationException("BackboneInfo expected")
    }
  }
}

import BackboneInfoJsonProtocol._
