package ru.biocad.ig.common.algorithms.geometry

import ru.biocad.ig.common.structures.geometry._

class DelaunayTesselation3 {

  val EPSILON = 0.1

  var simplices = collection.mutable.Set[Simplex]()
  var neighbours = collection.mutable.Map[Simplex, collection.mutable.Set[Simplex]]()
  var last_simplices : Seq[Simplex]= Seq()

  var adjacentByVertex   = collection.mutable.Map[GeometryVector, collection.mutable.Set[Simplex]]()
  var adjacentByEdge     = collection.mutable.Map[Set[GeometryVector], collection.mutable.Set[Simplex]]()
  var adjacentByTriangle = collection.mutable.Map[Set[GeometryVector], collection.mutable.Set[Simplex]]()

  var outerSet = collection.mutable.Map[Simplex, collection.mutable.Set[GeometryVector]]()

  def removeSimplex(s : Simplex) = {
    simplices.remove(s)
  }
  def appendPoint(new_point : GeometryVector) : Unit = {

  }
  def addSimplex(s: Simplex) = {
    simplices.add(s)
  }

  def prepareStartSimplex(points : Seq[GeometryVector]) : Simplex = {
    new Simplex(points.take(4))
  }
  /**helper method. returns furthest point for given simplex
  */
  def selectFurthest(simplex: Simplex, points : Seq[GeometryVector]) : GeometryVector = {
    points.maxBy(simplex.getPosition_(_))
  }

  def getBorderLine(simplices : Seq[Simplex]) : Seq[Set[GeometryVector]] = {
    val allRidges = simplices.flatMap(_.getTriangles())
    val uniqueRidges = allRidges.toSet.toSeq.diff(allRidges.diff(allRidges.toSet.toSeq).toSet.toSeq)
    uniqueRidges
  }
  /**this implementation should be qhull-based*/
  def makeTesselation(points : Seq[GeometryVector]) : Unit = {
    val startSimplex = prepareStartSimplex(points.distinct)
    addSimplex(startSimplex)
    var unprocessedPoints = points.distinct.toSet
    for (simplex <- simplices) {
      for (point <- unprocessedPoints) {
        val dist = simplex.getPosition_(point)
        println(dist)
        //FIX: to small value
        if (dist > 0.001) {
          outerSet.getOrElseUpdate(simplex, collection.mutable.Set()) += point
          unprocessedPoints -= point
        }
      }
    }
    for ((simplex, outerPoints) <- outerSet) {
      var point = selectFurthest(simplex, outerPoints.toSeq)
      var visibleSet = collection.mutable.Set[Simplex](simplex)
      for (neighbour <- neighbours(simplex)) {
        val dist = neighbour.getPosition_(point)
        //FIX: change to comparision with small value
        if (dist > 0.001) {
          visibleSet += neighbour
        }
      }

      val ridges = getBorderLine(visibleSet.toSeq)
      var newSimplices = collection.mutable.Set[Simplex]()
      for (ridge <- ridges) {
        val simplex = new Simplex(ridge.toSeq :+ point)
        newSimplices += simplex
      }
      //TODO: add links to newly created simplices from their neighbours
      //TODO: build outside points set for all visible Set once again
      for (newSimplex <- newSimplices) {
        //do s
      }


    }
    //println("removing")
    //simplices = removeBoundingSimplex(startSimplex)
  }
}
