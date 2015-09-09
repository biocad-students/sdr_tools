package ru.biocad.ig.alascan.constants.energy_terms

import spray.json._

case class EPair(
      val abaa : Map[String, Double],
      val apab : Map[String, Map[String, Double]]) {
  def get(aa1 : String, aa2 : String) : Double = {
    apab(aa1)(aa2)
  }
}

import DefaultJsonProtocol._
object EPairJsonProtocol extends DefaultJsonProtocol {
  implicit val EPairJsonFormat = jsonFormat2(EPair)
}

import EPairJsonProtocol._
