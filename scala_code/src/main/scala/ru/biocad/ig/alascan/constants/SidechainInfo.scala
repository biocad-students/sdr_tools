package ru.biocad.ig.alascan.constants

import ru.biocad.ig.common.io.pdb.PDBAtomInfo
import ru.biocad.ig.common.structures.aminoacid.{SimplifiedAminoAcid, Rotamer}
import ru.biocad.ig.common.structures.geometry.{GeometryVector, AminoacidUtils}
import java.util.Random

case class SidechainInfo(
  val representatives : Seq[Rotamer],
  val amounts : Seq[Int],
  var total : Int) extends AminoacidFragment {

    /** Updates current aminoacid's atom coordinates (for sidechain) with new ones and returns them
      * @param aminoacid object containing original PDBAtomInfo structures
      * Following parameters (x, y, z) -- describe local coordinate system's axes directions.
      * They play role in convertion to global coordinate system.
      * @return a list (Seq) of atom descriptions as PDBAtomInfo objects with updated coordinates
      */
    override def getPDBAtomInfo(aminoacid : SimplifiedAminoAcid,
            x : GeometryVector, y : GeometryVector, z : GeometryVector) : Seq[PDBAtomInfo] = {
        val a = representatives.sortWith({(a, b) =>
          (aminoacid.rotamer.center - a.center).lengthSquared < (aminoacid.rotamer.center - b.center).lengthSquared})
        //println(a.head.atoms)
        a.head.atoms.map({case (k, v) =>
        (k, AminoacidUtils.getGlobalCoordinates(Seq(x, y, z), v.toSeq) )}).map({
          case (k, v) => aminoacid.getUpdatedAtomInfo(k, v)
        }).toSeq
    }

    /** Replaces rotamer of given SimplifiedAminoAcid with some Rotamer from current library fragment
      *
      * @param aminoacidToModify aminoacid, which rotamer portion gets modified
      */
    def setRotamerFromLibrary(aminoacidToModify : SimplifiedAminoAcid) : Unit = {
        val a = representatives.sortWith({
          (a, b) => (aminoacidToModify.rotamer.center - a.center).lengthSquared <
          (aminoacidToModify.rotamer.center - b.center).lengthSquared
        })
        aminoacidToModify.rotamer = a.head
    }

    /** Finds nearest rotamer in library fragment and replaces current aa's rotamer to some other (randomly picked)
      *
      * @param aminoacidToModify aminoacid, which rotamer portion gets modified
      */
    def changeRotamerToRandom(aminoacidToModify : SimplifiedAminoAcid) : Unit = {
        val a = representatives.sortWith(
        {
          (a, b) =>
          (aminoacidToModify.rotamer.center - a.center).lengthSquared < (aminoacidToModify.rotamer.center - b.center).lengthSquared
        })
        aminoacidToModify.rotamer = Random.shuffle(a.tail).head
    }
}
