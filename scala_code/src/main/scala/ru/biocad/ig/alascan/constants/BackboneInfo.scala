package ru.biocad.ig.alascan.constants

import ru.biocad.ig.common.io.pdb.PDBAtomInfo
import ru.biocad.ig.common.structures.aminoacid.SimplifiedAminoacid
import ru.biocad.ig.common.structures.geometry.{GeometryVector, Lattice}
import ru.biocad.ig.common.algorithms.geometry.AminoacidUtils
import com.typesafe.scalalogging.slf4j.LazyLogging

/** Storage for set of given aminoacid fragments, forming backbone
  * @constructor creates such a storage (normally from json)
  * @param data Map containing mean coordinates for set of atoms forming the backbone.
  * Keys should be Strings: "C", "N", "O" - values for Protein Data Bank names for that atoms.
  * data's values are given in local coordinate system, with center in alpha-carbon coordinates and based on local topology.
  *
  */
case class BackboneInfo(val data : Map[String, GeometryVector]) extends AminoacidFragment with LazyLogging{

  /** Updates current aminoacid's atom coordinates (for backbone) with new ones and returns them
    * @param aminoacid object containing original PDBAtomInfo structures
    * Following parameters (x, y, z) -- describe local coordinate system's axes directions.
    * They play role in convertion to global coordinate system.
    * @return a list (Seq) of atom descriptions as PDBAtomInfo objects with updated coordinates
    */
  override def getPDBAtomInfo(aminoacid : SimplifiedAminoacid,
          x : GeometryVector, y : GeometryVector, z : GeometryVector,
          atomsMap : Map[String, PDBAtomInfo], meshSize : Double) : Seq[PDBAtomInfo] = {
      getCoordinatesMap(aminoacid, x, y, z, meshSize).map({
        case (k, v) => aminoacid.getUpdatedAtomInfo(k, v, atomsMap)
      }).toSeq
  }

  override def getCoordinatesMap(aminoacid : SimplifiedAminoacid,
          x : GeometryVector,
          y : GeometryVector,
          z : GeometryVector,
          meshSize : Double) : Map[String, GeometryVector] = {
      logger.debug(data.toString)
      data.map({
        case (k, v) => (k, AminoacidUtils.getGlobalCoordinates(Seq(x, y, z), v.coordinates, aminoacid.caInLatticeCoordinates*meshSize))
        //TODO : fix this
      })
  }
}
