package ru.biocad.ig.common.structures.geometry

import org.scalautils._
import Tolerance._
import TripleEquals._

import scala.math.sqrt
import ru.biocad.ig.common.algorithms.geometry.ManifoldUtils

sealed trait GeometryVector {
  val EPSILON = 0.0000001
  def coordinates : Seq[Double]
  def length : Double
  def lengthSquared : Double
  def dimensions : Int

  def roundedCoordinates : Seq[Int] = coordinates.map(_.toInt)

  def round = this match {
    case v : Vector => new Vector(coordinates.map(math.rint(_)))
    case _ => this
  }

  def unary_-() : GeometryVector = this match {
    case v : Vector => new Vector(coordinates.map(- _))
    case _ => this
  }

  def -(v : GeometryVector) : GeometryVector = v match {
    case vx : Vector => new Vector( (coordinates, v.coordinates).zipped map ( _ - _ ) )
    case _ => v
  }

  def +(v : GeometryVector) : GeometryVector = v match {
    case vx : Vector => new Vector( (coordinates, v.coordinates).zipped map ( _ + _ ) )
    case _ => v
  }

  def *(v : GeometryVector) : Double = ((coordinates, v.coordinates).zipped map ( _ * _ )).foldLeft(0.0) ( _ + _ )

  //tries to compute vector product for given pair of vectors, based on 1st 3 coordinates
  def ** (v : GeometryVector) : GeometryVector = new Vector(ManifoldUtils.getCofactorsVector(Seq(coordinates, v.coordinates)))

  def normalize() : GeometryVector = this/this.length

  def *(multiplier : Double) : GeometryVector = new Vector( coordinates.map( multiplier * _ ) )
  def /(divider : Double) : GeometryVector = new Vector( coordinates.map( _ / divider) )

  def distanceTo(v : GeometryVector) = (this - v).length
  def angleTo(v : GeometryVector) = math.toDegrees(math.acos((this * v) / (this.length*v.length)))

  def toSeq : Seq[Double]
  def lifted : GeometryVector // = coordinates ++ Seq(lengthSquared)
  def unlifted : GeometryVector

  /** the following method returns true if point is above hyperplane defined by given simplex
  (points below hyperplane are detected with given simplex's lowerPoint)
  */
  def isAbove(hyperplane : Simplex) : Boolean = {
    !hyperplane.hasVertex(this) && (hyperplane.getDistance(this) < -EPSILON) //FIX: change to comparision with small value
  }
  def isBelow(hyperplane : Simplex) : Boolean = {
    !hyperplane.hasVertex(this) && (hyperplane.getDistance(this) > EPSILON)
  }

  override def equals(other : Any) : Boolean = false
  override def hashCode: Int = (math.round(coordinates.reduceLeft(_ + _*100))).toInt + dimensions
}


case class Vector(val coordinates : Seq[Double]) extends GeometryVector  {
  override val dimensions = coordinates.size

  override lazy val lengthSquared : Double = (coordinates, coordinates).zipped.map( _ * _ ).reduceLeft( _ + _ )
  override lazy val length : Double = sqrt(lengthSquared)

  override def toSeq : Seq[Double] = Seq(1.0) ++ coordinates ++ Seq(lengthSquared)

  override def lifted : GeometryVector = new Vector(coordinates ++ Seq(lengthSquared))
  override def unlifted : GeometryVector = new Vector(coordinates.init) // TODO: add empty list check

  override def equals(other : Any) : Boolean = other match {
    case v : Vector => {
      (dimensions == v.dimensions) && (coordinates, v.coordinates).zipped.forall(_ === _ +- EPSILON)
    }
    case _ => false
  }

  //override def toString = coordinates.mkString("Vector (", ", ", ")")
  override def toString = coordinates.mkString("custom Vector[", ", ", "]")
}


object Vector2d {
  def apply(x : Double, y : Double) = new Vector(Seq(x, y))
}


case class InfiniteVector(val dimensions : Int) extends GeometryVector {
  override val coordinates : Seq[Double] = Seq.fill(dimensions)(0)
  override val lengthSquared : Double = 1.0
  override val length : Double = 1.0


  override def equals(other : Any) : Boolean = other match {
    case v : InfiniteVector => dimensions == v.dimensions
    case _ => false
  }
  override def - (v : GeometryVector) : GeometryVector = this
  override def + (v : GeometryVector) : GeometryVector = this
  override def *(v : GeometryVector) : Double = 0.0
  override def *(multiplier : Double) : GeometryVector = this

  override def toSeq : Seq[Double] = Seq(0.0) ++ coordinates ++ Seq(lengthSquared)

  override def lifted : GeometryVector = new InfiniteVector(dimensions + 1)//TODO: check if this is correct definition
  override def unlifted : GeometryVector = new InfiniteVector(dimensions - 1) // TODO: add empty list check

}


object Vector3d {
  def apply(x : Double, y : Double, z : Double = 0.0) = new Vector(Seq(x, y, z))
}
