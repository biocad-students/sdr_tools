package ru.biocad.ig.common.structures.geometry

import ru.biocad.ig.common.algorithms.geometry.ManifoldUtils

import collection.immutable.Set
import com.typesafe.scalalogging.slf4j.LazyLogging

object PointPosition extends Enumeration {
  type PointPosition = Value
  val LaysInside, LaysOnNSphere, LaysOutside = Value
}

import PointPosition._


class Simplex(val vertices : Seq[GeometryVector],
        val lowerPoint : GeometryVector,
        val innerPoint : GeometryVector
      ) extends LazyLogging {
  val EPSILON = 0.0000001
  val dimensions : Int = vertices.head.dimensions
  //var innerPoint : GeometryVector = vertices.reduceLeft(_ + _) /vertices.size
  //var lowerPoint : GeometryVector = vertices.reduceLeft(_ + _) /vertices.size

  def this(vertices : Seq[GeometryVector], lowerPoint : GeometryVector) = this(vertices, lowerPoint, vertices.reduceLeft(_ + _)/vertices.size)
  def this(vertices : Seq[GeometryVector]) = this(vertices, vertices.map(_.lifted).reduceLeft(_ + _)/vertices.size)

  val (normals: Seq[Double], dNorm : Double, distFunc) = ManifoldUtils.getSimplexNormalEquationParameters(vertices, EPSILON)

  private lazy val testFunc : Seq[Double] => Double = ManifoldUtils.getCofactors(vertices.map(_.toSeq))
  //private lazy val distFunc : Seq[Double] => Double =
  def reorient() : Simplex = {
    //return this
    if (innerPoint.lifted.isBelow(this) && vertices.size >= 2) {
      logger.debug("reorienting")
      new Simplex(Seq(vertices.tail.head, vertices.head) ++ vertices.tail.tail,
          lowerPoint, innerPoint)
    }
    else {
    this
    }
  }
  def isInLowerConvHull() : Boolean = {
    logger.debug("in lower conv hull check, got " + normals.last +" and "+
      math.signum(distFunc(innerPoint.lifted))  + " and " + innerPoint.lifted.isAbove(this).toString)
    //vertices.foreach(logger.debug)
    //logger.debug(normals)
    //logger.debug(innerPoint)
    //innerPoint.lifted.isAbove(this)
    //math.signum(distFunc(innerPoint.lifted)) > EPSILON //&& normals.last < 0
    math.signum(distFunc(innerPoint.lifted)) < EPSILON
    //normals.last*math.signum(distFunc(innerPoint.lifted)) < EPSILON
    //normals.last*math.signum(distFunc(lowerPoint)) < - EPSILON
  }
  //FIX: change naming of this later and also return distance instead
  def getPosition_(v : GeometryVector) : Double = {
    //normals.last*math.signum(distFunc(v.lifted))
    getDistance(v)
    //testFunc(v.toSeq) * math.signum(testFunc(InfiniteVector(dimensions).toSeq))
  }
  def getPosition(v : GeometryVector) : PointPosition = {
    val result : Double = getPosition_(v)//testFunc(v.toSeq) * flag//math.signum(testFunc(lowerPoint.toSeq))//InfiniteVector(dimensions).toSeq))
    //logger.debug("result in getPosition: " + result)
    if (v.isAbove(this)) {
      logger.debug("Lays inside with %f %f %f".format(normals.last, distFunc(innerPoint.lifted),
      getDistance(v)))
      LaysInside
    }
    else {
      if (v.isBelow(this)) {
        logger.debug("Lays outside with %f %f %f".format(normals.last, distFunc(innerPoint.lifted), getDistance(v)))

        LaysOutside
      }
      else {
        LaysOnNSphere
      }

    }
    //if (result.abs <= EPSILON) //FIX: there should be comparision with near-zero value
    //LaysOnNSphere
    //else {
    //  if (result > EPSILON) LaysOutside
    //  else LaysInside
    //}
  }

  def getDistance(point : GeometryVector) : Double = {
    //normals.last*math.signum(distFunc(innerPoint.lifted))
    if (point.dimensions < lowerPoint.dimensions){
      distFunc(point.lifted) * math.signum(distFunc(lowerPoint))
    }
    else {
      distFunc(point) * math.signum(distFunc(lowerPoint))
    }
  }

  def isFlat : Boolean = dNorm.abs < EPSILON

  def hasVertex(point : GeometryVector) : Boolean = vertices.find(_.equals(point)) != None

  def iterateThroughVertices() = new GeometryVectorIterator(vertices)

  def getLineSegments() : Seq[Set[GeometryVector]] = {
    vertices.tails.flatMap {
      case Seq() => Seq[Set[GeometryVector]]()
      case Seq(x) => Seq[Set[GeometryVector]]()
      case tailSequence : Seq[GeometryVector] => {
        tailSequence.tail.map(Set(tailSequence.head, _))
    }}.toSeq
  }

  def getTriangles() : Seq[Set[GeometryVector]] = {
    vertices.tails.flatMap {
      case Seq() => Seq[Set[GeometryVector]]()
      case Seq(x) => Seq[Set[GeometryVector]]()
      case tailSequence : Seq[GeometryVector] => {
        tailSequence.tail.tails.flatMap {
          case Seq() => Seq[Set[GeometryVector]]()
          case Seq(x) => Seq[Set[GeometryVector]]()
          case tailSequence2: Seq[GeometryVector] => {
            tailSequence2.tail.map(Set(tailSequence.head, tailSequence2.head, _))
          }
        }
    }}.toSeq
  }

  // in common, method differs from getTriangles - for (d+1)-dimensional simplex in d-space, it returns d-dimensional ridge
  def getRidges() : Iterator[Set[GeometryVector]] = iterateThroughVertices().map({
    case s : (GeometryVector, Seq[GeometryVector]) => s._2.toSet
    })

  override def equals(other : Any) : Boolean = other match {
    case v : Simplex => v.vertices.diff(vertices).isEmpty
    case _ => false
  }
  override def hashCode: Int = {
    vertices.sortWith(_.hashCode < _.hashCode).map(_.hashCode).reduceLeft(_ + _ * 100)
  }

  override def toString = vertices.mkString("\n[ \n  ", ", \n  ", " ],") + innerPoint.toString + " " +lowerPoint.toString
//  override def toString = vertices.mkString("\nSimplex[ \n  ", ", \n  ", " ]")
}
//todo: check if it useful to use getPosition for checking point position relative to different primitives
class Triangle(a : GeometryVector, b : GeometryVector, c : GeometryVector) extends Simplex(Seq(a, b, c, InfiniteVector(3))) {
  override def getPosition(v : GeometryVector) : PointPosition = {
    LaysOnNSphere //unimplemented, but surely we can make sphere by 3 points and another point
  }
}

case class Tetrahedra(
    a : GeometryVector,
    b : GeometryVector,
    c : GeometryVector,
    d : GeometryVector
  ) extends Simplex(Set(a, b, c, d).toSeq) {

}
