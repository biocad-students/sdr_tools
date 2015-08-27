package ru.biocad.ig.alascan.constants

case class RotamerLibrary(
  val data : Map[String, Map[Int, Map[Int, Map[Int, RotamerInfo]]]],
  val meshSize : Double = 1.0,
  val threshold = 1.7) {

}
