package ru.biocad.ig.common.structures.aminoacid

import ru.biocad.ig.common.io.pdb.PDBAtomInfo

import ru.biocad.ig.common.structures.geometry._

case class RotamerInfo(val atoms : Map[String, GeometryVector], val rotamer : GeometryVector) {
}
