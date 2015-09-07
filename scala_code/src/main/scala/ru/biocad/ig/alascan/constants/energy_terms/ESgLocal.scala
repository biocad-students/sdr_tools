package ru.biocad.ig.alascan.constants.energy_terms

import spray.json._
import DefaultJsonProtocol._
import ru.biocad.ig.common.structures.aminoacid.SimplifiedAminoAcid

case class ESgLocal(
      val local12 : Map[String, Map[String, Array[Double]]],
      val local13 : Map[String, Map[String, Array[Double]]],
      val local14 : Map[String, Map[String, Array[Double]]],
      val local15 : Map[String, Map[String, Array[Double]]]) {
  def get(aa1 : SimplifiedAminoAcid, aa2 : SimplifiedAminoAcid, distance : Int) : Double = {
    if (!local12.contains(aa1.name))
      return ??? //TODO: throw some error
    if (!local12.getOrElse(aa1.name, Map[String, Array[Double]]()).contains(aa2.name))
      return get(aa2, aa1, distance)

    distance match {
      case 1 => local12(aa1.name)(aa2.name)(0)
      case 2 => local13(aa1.name)(aa2.name)(0)
      case 3 => local14(aa1.name)(aa2.name)(0)
      case 4 => local15(aa1.name)(aa2.name)(0)
      case _ => 0.0
    }
  }
}

object ESgLocalJsonProtocol extends DefaultJsonProtocol {
  implicit val ESgLocalFormat = jsonFormat4(ESgLocal)
}

import ESgLocalJsonProtocol._
