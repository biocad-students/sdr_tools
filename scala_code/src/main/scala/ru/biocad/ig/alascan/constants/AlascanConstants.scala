package ru.biocad.ig.alascan.constants

case class BackboneInfo(
  val data : Map[String, Map[Int, Map[Int, Map[Int, Map[String, Seq[Double]]]]]],
  val meshSize : Double = 1.0) {
    def restoreCoordinates(aminoacid : String,
      d1 : Double,
      d2 : Double,
      d3 : Double) = {
      val i1 = math.round(d1 / meshSize).toInt
      val i2 = math.round(d2 / meshSize).toInt
      val i3 = math.round(d3 / meshSize).toInt
      data(aminoacid)(i1)(i2)(i3)
      //TODO: add existance check and lookup for nearest point - or at least smth located near
    }
}
