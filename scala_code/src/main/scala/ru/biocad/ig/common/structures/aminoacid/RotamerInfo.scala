package ru.biocad.ig.common.structures.aminoacid


import ru.biocad.ig.common.structures.geometry._

case class RotamerInfo(atoms : Map[String, GeometryVector], rotamer : GeometryVector) {
}
