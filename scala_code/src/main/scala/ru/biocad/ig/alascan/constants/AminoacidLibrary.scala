package ru.biocad.ig.alascan.constants

case class AminoacidLibrary[T](
  val data : Map[String, Map[Int, Map[Int, Map[Int, T]]]],
  val meshSize : Double = 1.0,
  val threshold : Double = 0.0)(implicit m: scala.reflect.Manifest[T]) {
    def restoreInfo(aminoacid : String,
      d1 : Double,
      d2 : Double,
      d3 : Double) : T = {
      val i1 = math.round(d1 / meshSize).toInt
      val i2 = math.round(d2 / meshSize).toInt
      val i3 = math.round(d3 / meshSize).toInt

      val m1 : Map[Int, Map[Int, T]] = data(aminoacid).getOrElse(i1, data(aminoacid).getOrElse(
        data(aminoacid).keys.minBy(x => math.abs(x - i1)),
        Map[Int, Map[Int, T]]()))
      val m2 : Map[Int, T] = m1.getOrElse(i2, m1.getOrElse(m1.keys.minBy(x => math.abs(x - i2)),
        Map[Int, T]()))
      val m3 = m2.getOrElse(i3, m2.getOrElse(m2.keys.minBy(x => math.abs(x - i3)) , m.erasure.newInstance().asInstanceOf[T]))
      m3
      //TODO: add existance check and lookup for nearest point - or at least smth located near
    }
}
