package ru.biocad.ig.alascan.constants

import ru.biocad.ig.common.io.pdb.PDBAtomInfo
import ru.biocad.ig.common.structures.aminoacid.{SimplifiedAminoacid, RotamerInfo}
import ru.biocad.ig.common.structures.geometry.GeometryVector
import ru.biocad.ig.common.algorithms.geometry.AminoacidUtils
import scala.util.Random

import com.typesafe.scalalogging.slf4j.LazyLogging


case class SidechainInfo (
  val representatives : Seq[RotamerInfo],
  val amounts : Seq[Int],
  var total : Int) extends AminoacidFragment with LazyLogging {

    def findNearestRepresentativeByDistanceTo(rotamer : GeometryVector) : RotamerInfo =
      representatives.minBy(a => (rotamer - a.rotamer).lengthSquared )


    /** Updates current aminoacid's atom coordinates (for sidechain) with new ones and returns them
      * @param aminoacid object containing original PDBAtomInfo structures
      * Following parameters (x, y, z) -- describe local coordinate system's axes directions.
      * They play role in convertion to global coordinate system.
      * @return a list (Seq) of atom descriptions as PDBAtomInfo objects with updated coordinates
      */
    override def getPDBAtomInfo(aminoacid : SimplifiedAminoacid,
            x : GeometryVector, y : GeometryVector, z : GeometryVector,
            atomsMap : Map[String, PDBAtomInfo]) : Seq[PDBAtomInfo] = {
        val a = findNearestRepresentativeByDistanceTo(aminoacid.rotamer)
        a.atoms.map({case (k, v) =>
        (k, AminoacidUtils.getGlobalCoordinates(Seq(x, y, z), v.coordinates, aminoacid.ca*1.22) )}).map({
          case (k, v) => aminoacid.getUpdatedAtomInfo(k, v, atomsMap)
        }).toSeq

    }

    override def getCoordinatesMap(aminoacid : SimplifiedAminoacid,
            x : GeometryVector,
            y : GeometryVector,
            z : GeometryVector) : Map[String, GeometryVector] = {
        val a = findNearestRepresentativeByDistanceTo(aminoacid.rotamer)
        a.atoms.map({
          case (k, v) => (k, AminoacidUtils.getGlobalCoordinates(Seq(x, y, z), v.coordinates, aminoacid.ca * LatticeConstants.MESH_SIZE) )
        })
    }

    /** Replaces rotamer of given SimplifiedAminoacid with some Rotamer from current library fragment
      *
      * @param aminoacidToModify aminoacid, which rotamer portion gets modified
      */
    def setRotamerFromLibrary(aminoacidToModify : SimplifiedAminoacid) : SimplifiedAminoacid = {
        if (representatives.size == 0)
            return aminoacidToModify
        val a = findNearestRepresentativeByDistanceTo(aminoacidToModify.rotamer)
        new SimplifiedAminoacid(aminoacidToModify.name, aminoacidToModify.ca, a.rotamer)
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
      *  val sidechainInfo = rotamerLibrary.restoreAminoacidInfo(aaToModify, d1, d2, d3)
      *  sidechainInfo.changeRotamerToRandom(a2)
      * }
      * }}}
      */
    def changeRotamerToRandom(aminoacidToModify : SimplifiedAminoacid) : SimplifiedAminoacid = {
      logger.info(representatives.size.toString)
        if (representatives.size < 2)
            return aminoacidToModify
        val a = findNearestRepresentativeByDistanceTo(aminoacidToModify.rotamer)
        val index = Random.nextInt(representatives.length - 1)
        val rest = representatives.filterNot(_ == a)(index)
        new SimplifiedAminoacid(aminoacidToModify.name, aminoacidToModify.ca, rest.rotamer)
    }

    def changeRotamerToRandom(rotamerToModify : GeometryVector) : GeometryVector = {
        if (representatives.size < 1)
            return rotamerToModify
        //val a = findNearestRepresentativeByDistanceTo(rotamerToModify)
        val index = Random.nextInt(representatives.length)
        val rest = representatives(index)
        rest.rotamer
    }
}
