package ru.biocad.ig.alascan.constants

import ru.biocad.ig.common.structures.aminoacid.Rotamer

case class RotamerInfo(
  val representatives : Seq[Rotamer], val amounts : Seq[Int], total : Int) {

}
