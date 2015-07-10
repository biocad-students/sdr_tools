package ru.biocad.ig.common.structures.geometry

import ru.biocad.ig.common.algorithms.geometry.ManifoldUtils

import collection.immutable.Set

object PointPosition extends Enumeration {
  type PointPosition = Value
  val LaysInside, LaysOnNSphere, LaysOutside = Value
}

import PointPosition._


/**
  * the following class iterates through given simple structures of the Simplex class
  */
class GeometryVectorIterator(input_sequence : Seq[GeometryVector]) extends Iterator[(GeometryVector, Seq[GeometryVector])] {
  var prefix : Seq[GeometryVector] = Seq()
  var tailSequence : Seq[GeometryVector] = input_sequence

  def hasNext : Boolean = !tailSequence.isEmpty

  def next() : (GeometryVector, Seq[GeometryVector]) = {
    if (hasNext) {
      val element : GeometryVector = tailSequence.head
      tailSequence = tailSequence.tail
      val allExceptElement : Seq[GeometryVector] = tailSequence ++ prefix
      prefix = prefix ++ Seq(element)
      (element, allExceptElement)
    } else {
      null
    }
  }
}


class Simplex(val vertices : Seq[GeometryVector]) {

  private lazy val testFunc : Seq[Double] => Double = ManifoldUtils.getCofactors(vertices.map(_.toSeq))

  //FIX: change naming of this later and also return distance instead
  def getPosition_(v : GeometryVector) : Double = {
    val result : Double = testFunc(v.toSeq) * math.signum(testFunc(InfiniteVector(v.dimensions).toSeq))
    result
  }
  def getPosition(v : GeometryVector) : PointPosition = {
    val result : Double = getPosition_(v)
    if ((result).abs <= 0.001) //FIX: there should be comparision with near-zero value
    LaysOnNSphere
    else {
      if (result > 0) LaysOutside
      else LaysInside
    }
  }

  def isFlat : Boolean = testFunc(InfiniteVector(vertices.head.dimensions).toSeq).abs < 0.001

  def hasVertex(point : GeometryVector) : Boolean = vertices.filter(_.equals(point)) != Seq()

  def iterateThroughVertices() = new GeometryVectorIterator(vertices)

  def getLineSegments() : Seq[Set[GeometryVector]] = {
    vertices.tails.flatMap {
      case Seq() => Seq[Set[GeometryVector]]()
      case Seq(x) => Seq[Set[GeometryVector]]()
      case tailSequence : Seq[GeometryVector] => {
        tailSequence.tail.map(Set(tailSequence.head, _))
    }}.toSeq
  }

  //FIX: rename to ridges - actually this returns ridges set
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

  override def equals(other : Any) : Boolean = other match {
    case v : Simplex => {
      v.vertices.diff(vertices).isEmpty
    }
    case _ => false
  }
override def toString = vertices.mkString("\n[ \n  ", ", \n  ", " ],")
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
