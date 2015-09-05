package ru.biocad.ig.alascan.constants.energy_terms

import spray.json._
import DefaultJsonProtocol._


case class E14aa(val aa1: String, val aa2:String, val e214 : Array[Double], val n14 : Int) {
}
object E14aaProtocol extends DefaultJsonProtocol {
  implicit val E14aaFormat = jsonFormat4(E14aa)
}
import E14aaProtocol._

case class E14(val data : Map[String, Map[String, E14aa]], val startIndex: Int) {
  /*method returns energy for current aminoacid pair and R14 distance for given aminoacid i*/
  def get(r14 : Double, aa1 : String, aa2 : String) : Double = {


  }
}

object E14JsonProtocol extends DefaultJsonProtocol {
  implicit val E14Format = jsonFormat2(E14)
}

import E14JsonProtocol._
