package ru.biocad.ig.alascan.constants

import ru.biocad.ig.common.structures.aminoacid.Rotamer
import ru.biocad.ig.common.io.pdb.PDBAtomInfo
import ru.biocad.ig.common.structures.aminoacid.SimplifiedAminoAcid
import ru.biocad.ig.common.structures.geometry.GeometryVector
import ru.biocad.ig.common.algorithms.geometry.AminoacidUtils

import java.util.Random

case class SidechainInfo(
  val representatives : Seq[Rotamer],
  val amounts : Seq[Int],
  var total : Int) extends AminoacidFragment {

    override def getPDBAtomInfo(aminoacid : SimplifiedAminoAcid,
            x : GeometryVector, y : GeometryVector, z : GeometryVector) : Seq[PDBAtomInfo] = {
        val a = representatives.sortWith({(a, b) =>
          (aminoacid.rotamer.center - a.center).lengthSquared < (aminoacid.rotamer.center - b.center).lengthSquared})
        //println(a.head.atoms)
        a.head.atoms.map({case (k, v) =>
        (k, AminoacidUtils.getGlobalCoordinates(Seq(x, y, z), v.toSeq) )}).map({
          case (k, v) => aminoacid.getUpdatedAtomInfo(k, v)
        }).toSeq
        //println("11111")
        /*data.map({
          case (k, v) => (k, AminoacidUtils.getGlobalCoordinates(Seq(x, y, z), v.toSeq))
        }).map({case (k, v) => aminoacid.getUpdatedAtomInfo(k, v) }).toSeq**/
        //??? //TODO: should implement
    }

    def setRotamerFromLibrary(aminoacidToModify : SimplifiedAminoAcid,
      d1:Double, d2:Double, d3:Double) = {
        val a = representatives.sortWith({a, b} => (aminoacidToModify.rotamer.center - a.center).lengthSquared <
        (aminoacidToModify.rotamer.center - b.center).lengthSquared)
        aminoacidToModify.rotamer = a.head
      }
    def changeRotamerToRandom(aminoacidToModify : SimplifiedAminoAcid,
    d1:Double, d2:Double, d3:Double) = {
      val a = representatives.sortWith({a, b} => (aminoacidToModify.rotamer.center - a.center).lengthSquared <
      (aminoacidToModify.rotamer.center - b.center).lengthSquared)
      aminoacidToModify.rotamer = Random.shuffle(a.tail).head
    }
}
