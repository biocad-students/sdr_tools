package ru.biocad.ig.alascan.constants.energy_terms

import spray.json._

case class EOne(val nkb : Array[Int], val eone: Array[Double]) {
}
import DefaultJsonProtocol._
object EOneJsonProtocol extends DefaultJsonProtocol {
  implicit val EOneFormat = jsonFormat2(EOne)
}

import EOneJsonProtocol._
