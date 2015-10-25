package ru.biocad.ig.alascan.constants.energy_terms

import spray.json._
import DefaultJsonProtocol._
import ru.biocad.ig.common.structures.aminoacid.SimplifiedAminoacid

case class RotamerRadiusInfo(
      val aminoacids : Map[String, Int],
      val data : Map[String, Array[Array[Double]]],
      val phoc : Array[Int],
      val aract : Double,
      val cscal : Double,
      val acharge : Double,
      val eRepulsive : Double) {
  def getR(aa1 : String, aa2 : String, latticeSize:Double = 1.22) : Double = {
    //computed as dam(i,j) in information file
    math.pow(0.01 * aract * data(aa1)(0)(aminoacids(aa2)) / latticeSize, 2)
  }

  def getRrep(aa1 : String, aa2 : String, latticeSize:Double = 1.22) : Double = {
    //damr(i,j)
    val damr = math.pow(0.01 * cscal * data(aa1)(1)(aminoacids(aa2)) / latticeSize, 2)
    if (phoc(aminoacids(aa1))*phoc(aminoacids(aa2)) == 1)
      return acharge * damr
    damr
  }
}

object RotamerRadiusInfoJsonProtocol extends DefaultJsonProtocol {
  implicit val RotamerRadiusInfoFormat = jsonFormat7(RotamerRadiusInfo)
}

import RotamerRadiusInfoJsonProtocol._
