package ru.biocad.ig.alascan.constants

import ru.biocad.ig.common.io.pdb.PDBAtomInfo
import ru.biocad.ig.common.structures.aminoacid.{SimplifiedAminoacid, RotamerInfo, Rotamer}
import ru.biocad.ig.common.structures.geometry.GeometryVector
import ru.biocad.ig.common.algorithms.geometry.AminoacidUtils
import scala.util.Random

case class SidechainInfo(
  val representatives : Seq[RotamerInfo],
  val amounts : Seq[Int],
  var total : Int) extends AminoacidFragment {

    /** Updates current aminoacid's atom coordinates (for sidechain) with new ones and returns them
      * @param aminoacid object containing original PDBAtomInfo structures
      * Following parameters (x, y, z) -- describe local coordinate system's axes directions.
      * They play role in convertion to global coordinate system.
      * @return a list (Seq) of atom descriptions as PDBAtomInfo objects with updated coordinates
      */
    override def getPDBAtomInfo(aminoacid : SimplifiedAminoacid,
            x : GeometryVector, y : GeometryVector, z : GeometryVector,
            atomsMap : Map[String, PDBAtomInfo]) : Seq[PDBAtomInfo] = {
        val a = representatives.sortWith({(a, b) =>
          (aminoacid.rotamer.center - a.center).lengthSquared < (aminoacid.rotamer.center - b.center).lengthSquared})
        //println(a.head.atoms)


        a.head.atoms.map({case (k, v) =>
        (k, AminoacidUtils.getGlobalCoordinates(Seq(x, y, z), v.toSeq) )}).map({
          case (k, v) => aminoacid.getUpdatedAtomInfo(k, v, atomsMap)
        }).toSeq

    }

    /** Replaces rotamer of given SimplifiedAminoacid with some Rotamer from current library fragment
      *
      * @param aminoacidToModify aminoacid, which rotamer portion gets modified
      */
    def setRotamerFromLibrary(aminoacidToModify : SimplifiedAminoacid) : SimplifiedAminoacid = {
        if (representatives.size == 0)
            return aminoacidToModify
        val a = representatives.sortWith({
          (a, b) => (aminoacidToModify.rotamer.center - a.center).lengthSquared <
          (aminoacidToModify.rotamer.center - b.center).lengthSquared
        })
        new SimplifiedAminoacid(aminoacidToModify.name, aminoacidToModify.ca, new Rotamer(a.head.center))
    }

    /** Finds nearest rotamer in library fragment and replaces current aa's rotamer to some other (randomly picked)
      *
      * @param aminoacidToModify aminoacid, which rotamer portion gets modified
      *
      * Following method implementation demonstrates usage example:
      * {{{
      * import ru.biocad.ig.alascan.constants.{AminoacidLibrary, SidechainInfo}
      *
      * def moveRotamer(structure : Seq[SimplifiedAminoacid], position : Int ) : SimplifiedAminoacid = {
      *  val (a1, a2, a3, a4) = Seq(position - 1, position, position + 1, position + 2).map({i=>structure(i)})
      *  val (d1, d2, d3) = AminoacidUtils.getDistances(a1.ca, a2.ca, a3.ca, a4.ca)
      *  val (x, y, z) = AminoacidUtils.getLocalCoordinateSystem(a1.ca, a2.ca, a3.ca, a4.ca)
      *  val sidechainInfo = rotamerLibrary.restoreAminoAcidInfo(aaToModify, d1, d2, d3)
      *  sidechainInfo.changeRotamerToRandom(a2)
      * }
      * }}}
      */
    def changeRotamerToRandom(aminoacidToModify : SimplifiedAminoacid) : SimplifiedAminoacid = {
        if (representatives.size < 2)
            return aminoacidToModify
        val a = representatives.sortWith(
        {
          (a, b) =>
          (aminoacidToModify.rotamer.center - a.center).lengthSquared < (aminoacidToModify.rotamer.center - b.center).lengthSquared
        })
        new SimplifiedAminoacid(aminoacidToModify.name, aminoacidToModify.ca, new Rotamer(Random.shuffle(a.tail).head.center))
    }
}
