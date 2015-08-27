package ru.biocad.ig.alascan.constants
/**this is duplicate class for loading rotamers from database. old class will be removed*/package ru.biocad.ig.alascan.constants

case class Rotamer(val atoms : Map[String, GeometryVector], val center: GeometryVector) {

}
