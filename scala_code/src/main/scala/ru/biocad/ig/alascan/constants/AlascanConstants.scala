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

      val m1 : Map[Int, Map[Int, Map[String, Seq[Double]]]] = data(aminoacid).getOrElse(i1, data(aminoacid).getOrElse(
        data(aminoacid).keys.minBy(x => math.abs(x - i1)),
        Map[Int, Map[Int, Map[String, Seq[Double]]]]()))
      val m2 : Map[Int, Map[String, Seq[Double]]] = m1.getOrElse(i2, m1.getOrElse(m1.keys.minBy(x => math.abs(x - i2)),
        Map[Int, Map[String, Seq[Double]]]()))
      val m3 = m2.getOrElse(i3, m2.getOrElse(m2.keys.minBy(x => math.abs(x - i3)),
        Map[String, Seq[Double]]()))
      m3
      //TODO: add existance check and lookup for nearest point - or at least smth located near
    }
}
