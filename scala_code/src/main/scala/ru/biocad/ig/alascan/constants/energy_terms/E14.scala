package ru.biocad.ig.alascan.constants.energy_terms

import spray.json._
import DefaultJsonProtocol._


case class E14aa(val aa1: String, val aa2:String, val e214 : Array[Double], val n14 : Int) {
}
object E14aaProtocol extends DefaultJsonProtocol {
  implicit val E14aaFormat = jsonFormat4(E14aa)
}
import E14aaProtocol._

case class E14(
      val data : Map[String, Map[String, E14aa]],
      val startIndex : Int,
      val endIndex : Int,
      val binRightBorders : Seq[Int]) {

  def getValidIndex(r14 : Double) : Int = math.round(r14).toInt match {
    case i if i < 0 => 0
    case i if i <= endIndex => i - startIndex
    case _ => endIndex - startIndex
  }

  def getBinNumber(r14 : Double) : Int = {
    val x = math.round(r14).toInt
    binRightBorders.zipWithIndex.find({ c => x < c._1}) match {
        case Some(v) => v._2
        case _ => binRightBorders.size
      }
  }

  /*method returns energy for current aminoacid pair and R14 distance for given aminoacid i*/
  def get(r14 : Double, aa1 : String, aa2 : String) : Double = {
    if (!data.contains(aa1))
      return ??? //TODO: should return some error here
    else if (!data(aa1).contains(aa2)) {
       return get(r14, aa2, aa1)
    }

    val index = getValidIndex(r14)
    println(data(aa1)(aa2).e214(index))
      //println(getBinNumber(r14))
    ???
  }
}

object E14JsonProtocol extends DefaultJsonProtocol {
  implicit val E14Format = jsonFormat4(E14)
}

import E14JsonProtocol._
