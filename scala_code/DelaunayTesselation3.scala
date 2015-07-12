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

  //returns set of simplices in d+1, wich are ridges of simplex in d+2
  //shouldn't be degenerate
  def prepareStartSimplex(points : Seq[GeometryVector]) : Seq[Simplex] = {
    (new Simplex(points.take(points.head.dimensions+2))).iterateThroughVertices().map({
      case s : (GeometryVector, Seq[GeometryVector]) => {
        val simplex = new Simplex(s._2)
        simplex.lowerPoint = s._1
        simplex
        }
    }).toSeq
  }
  /**helper method. returns furthest point for given simplex
  */
  def selectFurthest(simplex: Simplex, points : Seq[GeometryVector]) : GeometryVector = {
    points.maxBy(simplex.getPosition_(_))
  }
  def addNeighbours(simplex : Simplex) = {
    for (ridge <- simplex.getRidges()) {
      adjacentByTriangle.getOrElseUpdate(ridge, collection.mutable.Set()) += simplex
    }
  }

  def addNeigboursAtInfinity(simplex: Simplex) = {
    val farthest_point = InfiniteVector(simplex.vertices.head.dimensions)
    for (ridge <- simplex.getRidges()) {
      println("triangle:")
      println(ridge)
      val s = new Simplex(ridge.toSeq :+ farthest_point)
      addSimplex(s)
      addNeighbours(s)
    }
  }

  def getNeighbours(simplex: Simplex) : Seq[Simplex] = {
    simplex.getRidges().flatMap(adjacentByTriangle.getOrElse(_, Seq())).toSeq.distinct.diff(Seq(simplex))
  }
  def removeNeighbours(simplex : Simplex) = {
    for (triangle <- simplex.getRidges()) {
      if (adjacentByTriangle.contains(triangle))
        adjacentByTriangle(triangle) -= simplex
    }
  }
  def getBorderLine(simplices : Seq[Simplex]) : Seq[Set[GeometryVector]] = {
    val allRidges = simplices.flatMap(_.getRidges())
    val uniqueRidges = allRidges.toSet.toSeq.diff(allRidges.diff(allRidges.toSet.toSeq).toSet.toSeq)
    uniqueRidges
  }
  def getLowerConvexHull(simplices : collection.mutable.Set[Simplex]) : collection.mutable.Set[Simplex] = {
    simplices.filter({
      case s : Simplex => s.getPosition_(new InfiniteVector(s.vertices.head.dimensions)) < - 0.001
    })
  }

  /**this implementation should be qhull-based*/
  def makeTesselation(points : Seq[GeometryVector]) : Unit = {
    val startSimplices = prepareStartSimplex(points.distinct)
    for (startSimplex <- startSimplices) {
      addSimplex(startSimplex)
      addNeighbours(startSimplex)
    }
    //addNeigboursAtInfinity(startSimplex)
    println("starting simplex received")
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
    println("processing outer set...")
    while (!outerSet.isEmpty) {
      var (simplex, outerPoints) = outerSet.head
      var point = selectFurthest(simplex, outerPoints.toSeq)
      var visibleSet = collection.mutable.Set[Simplex](simplex)
      println(1)
//      if (neighbours.contains(simplex)){
        for (neighbour <- getNeighbours(simplex)) {
          val dist = neighbour.getPosition_(point)
          //FIX: change to comparision with small value
          if (dist > 0.001) {
            visibleSet += neighbour
          }
        }
//      }
      println(2)

      val ridges = getBorderLine(visibleSet.toSeq)
      var newSimplices = collection.mutable.Set[Simplex]()
      for (ridge <- ridges) {
        val simplex = new Simplex(ridge.toSeq :+ point)
        simplex.lowerPoint = visibleSet.head.lowerPoint
        newSimplices += simplex

      }
      //TODO: add links to newly created simplices from their neighbours
      //TODO: build outside points set for all visible Set once again
      var unprocessedPoints = Set[GeometryVector]()
      for (simplex <- visibleSet) {
        if (outerSet.contains(simplex))
        unprocessedPoints ++= outerSet(simplex)
      }
      for (newSimplex <- newSimplices) {
        for (point <- unprocessedPoints) {
          val dist = newSimplex.getPosition_(point)
          //println(dist)
          //FIX: to small value
          if (dist > 0.001) {
            outerSet.getOrElseUpdate(newSimplex, collection.mutable.Set()) += point
            unprocessedPoints -= point
          }
        }
        addSimplex(newSimplex)
        addNeighbours(newSimplex)
      }
      for (simplex <- visibleSet) {
        removeSimplex(simplex)
        outerSet.remove(simplex)
        removeNeighbours(simplex)
      }

    }
    println("all simplices after processing")
    simplices.foreach(println)
    println("all neighbours after processing")
    adjacentByTriangle.foreach(println)
    println("end of processing")
    simplices = getLowerConvexHull(simplices)
    //println("removing")
    //simplices = removeBoundingSimplex(startSimplex)
  }

}
