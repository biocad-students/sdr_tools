package ru.biocad.ig.alascan.constants

import ru.biocad.ig.common.io.pdb.PDBAtomInfo
import ru.biocad.ig.common.structures.aminoacid.SimplifiedAminoAcid
import ru.biocad.ig.common.structures.geometry.GeometryVector

trait AminoacidFragment {

  /** Updates current aminoacid's atom coordinates (for some fragment) with new ones and returns them
    * @param aminoacid object containing original PDBAtomInfo structures
    * Following parameters (x, y, z) -- describe local coordinate system's axes directions.
    * They play role in convertion to global coordinate system.
    * @return a list (Seq) of atom descriptions as PDBAtomInfo objects with updated coordinates
    */
  def getPDBAtomInfo(aminoacid : SimplifiedAminoAcid,
    x : GeometryVector, y : GeometryVector, z : GeometryVector) : Seq[PDBAtomInfo]
}
