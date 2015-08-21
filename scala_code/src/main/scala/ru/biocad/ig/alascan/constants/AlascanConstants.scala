package ru.biocad.ig.alascan.constants

case class BackboneInfo(
  val data : Map[String, Map[String, Map[String, Map[String, Map[String, Seq[Double]]]]]],
  val meshSize : Double) {

}
