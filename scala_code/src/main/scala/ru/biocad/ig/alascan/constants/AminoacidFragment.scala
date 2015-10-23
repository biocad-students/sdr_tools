package ru.biocad.ig.alascan.constants

import ru.biocad.ig.common.io.pdb.PDBAtomInfo
import ru.biocad.ig.common.structures.aminoacid.SimplifiedAminoAcid
import ru.biocad.ig.common.structures.geometry.GeometryVector

trait AminoacidFragment {
  def getPDBAtomInfo(aminoacid : SimplifiedAminoAcid,
    x : GeometryVector, y : GeometryVector, z : GeometryVector) : Seq[PDBAtomInfo]
}
