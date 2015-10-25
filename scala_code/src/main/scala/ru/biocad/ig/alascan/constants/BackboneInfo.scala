package ru.biocad.ig.alascan.constants

import ru.biocad.ig.common.io.pdb.PDBAtomInfo
import ru.biocad.ig.common.structures.aminoacid.SimplifiedAminoacid
import ru.biocad.ig.common.structures.geometry.GeometryVector
import ru.biocad.ig.common.algorithms.geometry.AminoacidUtils

case class BackboneInfo(val data : Map[String, GeometryVector]) extends AminoacidFragment {
  
  /** Updates current aminoacid's atom coordinates (for backbone) with new ones and returns them
    * @param aminoacid object containing original PDBAtomInfo structures
    * Following parameters (x, y, z) -- describe local coordinate system's axes directions.
    * They play role in convertion to global coordinate system.
    * @return a list (Seq) of atom descriptions as PDBAtomInfo objects with updated coordinates
    */
  override def getPDBAtomInfo(aminoacid : SimplifiedAminoacid,
          x : GeometryVector, y : GeometryVector, z : GeometryVector) : Seq[PDBAtomInfo] = {
      data.map({
        case (k, v) => (k, AminoacidUtils.getGlobalCoordinates(Seq(x, y, z), v.toSeq))
      }).map({case (k, v) => aminoacid.getUpdatedAtomInfo(k, v) }).toSeq
  }
}
