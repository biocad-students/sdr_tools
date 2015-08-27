package ru.biocad.ig.alascan.constants

case class RotamerInfo(
  val representatives : Seq[Rotamer], val amounts : Seq[Int], total : Int) {

}
