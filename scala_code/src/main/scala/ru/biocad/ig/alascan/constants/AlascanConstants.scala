package ru.biocad.ig.alascan.constants

case class BackboneInfo(
  val data : Map[String, Map[Int, Map[Int, Map[Int, Map[String, Seq[Double]]]]]],
  val meshSize : Double) {

}
