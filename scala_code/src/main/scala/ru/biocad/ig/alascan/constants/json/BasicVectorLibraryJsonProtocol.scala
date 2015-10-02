package ru.biocad.ig.alascan.constants.json

import spray.json._
//import ru.biocad.ig.alascan.constants.{AminoacidLibrary, BackboneInfo}
import ru.biocad.ig.common.structures.geometry.{GeometryVector, Vector}
//import BackboneInfoJsonProtocol._

case class BasicVectorLibrary(val vectors : Seq[Seq[Double]]) {}

import DefaultJsonProtocol._

object BasicVectorLibraryJsonProtocol extends DefaultJsonProtocol {
  implicit val basicVectorLibraryJsonFormat = jsonFormat1(BasicVectorLibrary)
}

import BasicVectorLibraryJsonProtocol._
//
