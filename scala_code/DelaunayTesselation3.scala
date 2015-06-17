package ru.biocad.ig.common.algorithms.geometry

import ru.biocad.ig.common.structures.geometry._

class DelaunayTesselation3 {

  val EPSILON = 0.1

  var simplices : Set[Simplex] = Set()
  var last_simplices : Seq[Simplex]= Seq()

  var adjacentByVertex   = collection.mutable.Map[GeometryVector, collection.mutable.Set[Simplex]]()
  var adjacentByEdge     = collection.mutable.Map[Set[GeometryVector], collection.mutable.Set[Simplex]]()
  var adjacentByTriangle = collection.mutable.Map[Set[GeometryVector], collection.mutable.Set[Simplex]]()

  def removeSimplex(s : Simplex) = {

  }
  def appendPoint(new_point : GeometryVector) : Unit = {

  }
  def addSimplex(s: Simplex) = {

  }

  def prepareStartSimplex(points : Seq[GeometryVector]) : Simplex = {
    new Simplex(points.take(4))
  }
  def makeTesselation(points : Seq[GeometryVector]) : Unit = {
    val startSimplex = prepareStartSimplex(points.distinct)
    addSimplex(startSimplex)
    last_simplices = Seq(startSimplex)
    var l = 0
    points.drop(4).foreach (point=>{
      l += 1
      //if (l % 2 == 0)
        println("processing: " + l)
      appendPoint(point)
    })
    //println("removing")
    //simplices = removeBoundingSimplex(startSimplex)
  }
}
