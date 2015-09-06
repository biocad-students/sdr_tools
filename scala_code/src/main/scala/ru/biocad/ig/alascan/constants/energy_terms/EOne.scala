package ru.biocad.ig.alascan.constants.energy_terms

import spray.json._

case class EOne(val nkb : Array[Int], val eone: Array[Double]) {
  def get(aminoacid : String, numberOfContacts : Int) : Double = {
    val aminoacidNames = Seq("GLY",
      "ALA","SER","CYS","VAL","THR",
      "ILE","PRO", "MET","ASP","ASN","LEU", "LYS","GLU","GLN","ARG",
      "HIS","PHE","TYR","TRP","CYX").zipWithIndex.toMap
    val index = aminoacidNames(aminoacid)
    if (numberOfContacts > nkb(index))
    0.0
    else eone(index)
  }
}
import DefaultJsonProtocol._
object EOneJsonProtocol extends DefaultJsonProtocol {
  implicit val EOneFormat = jsonFormat2(EOne)
}

import EOneJsonProtocol._
