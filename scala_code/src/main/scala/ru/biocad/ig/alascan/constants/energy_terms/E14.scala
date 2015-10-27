package ru.biocad.ig.alascan.constants.energy_terms

import spray.json._
import DefaultJsonProtocol._

//FIX: naming according to original parameter names
//NOTE: according to MCDP.info, there should be some preprocessing here.
//currently I'm loading r14avg12.json and r14aa12.json separately, with no preprocessing
/*this class contains average values from r14avg12.json*/
case class E14avg(val aros : Array[Array[Double]],
                  val e14 : Array[Double],
                  val startIndex : Int,
                  val endIndex : Int,
                  val binRightBorders : Seq[Int]) {

  def getBinNumber(r14 : Double) : Int = {
    val x = math.round(r14).toInt
    binRightBorders.zipWithIndex.find({ c => x < c._1}) match {
        case Some(v) => v._2
        case _ => binRightBorders.size
      }
  }

  def get(r14 : Double, r14prev : Double) : Double = {
    return aros(getBinNumber(r14))(getBinNumber(r14prev))
  }
}

object E14avgJsonProtocol extends DefaultJsonProtocol {
  implicit val E14avgJsonFormat = jsonFormat5(E14avg)
}
import E14avgJsonProtocol._

/*this class contains values from r14aa12.json, it is hidden within E14 class*/
case class E14aa(val aa1 : String, val aa2 : String, val e214 : Array[Double], val n14 : Int) {
}
object E14aaJsonProtocol extends DefaultJsonProtocol {
  implicit val E14aaJsonFormat = jsonFormat4(E14aa)
}
import E14aaJsonProtocol._

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

  /*method returns energy for current aminoacid pair and R14 distance for given aminoacid i*/
  def get(r14 : Double, aa1 : String, aa2 : String) : Double = {
    if (!data.contains(aa1))
      return ??? //TODO: should return some error here
    else if (!data(aa1).contains(aa2)) {
       return get(r14, aa2, aa1)
    }
    return data(aa1)(aa2).e214(getValidIndex(r14))
  }
}

object E14JsonProtocol extends DefaultJsonProtocol {
  implicit val E14Format = jsonFormat4(E14)
}

import E14JsonProtocol._
