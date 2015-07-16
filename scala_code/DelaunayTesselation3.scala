package ru.biocad.ig.common.algorithms.geometry

import ru.biocad.ig.common.structures.geometry._

class DelaunayTesselation3 {

  val EPSILON = 0.001

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
    val pp = Seq(points.maxBy(_.coordinates(0)),
      points.minBy(_.coordinates(0)),
      points.maxBy(_.coordinates(1)),
      points.minBy(_.coordinates(1)),
      points.maxBy(_.coordinates(2)),
      points.minBy(_.coordinates(2)),
      points.maxBy(_.length),
      points.minBy(_.length)).distinct.take(points.head.dimensions + 2)
    println("start simplex:")
    println(pp)
    (new Simplex(pp)).iterateThroughVertices().map({
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
    val p = points.maxBy(-simplex.getPosition_(_))
    val p2 = points.minBy(- simplex.getPosition_(_))
    println("furthest point dist: " + simplex.getPosition_(p) +" , "+ simplex.getPosition_(p2))
    p
  }
  def addNeighbours(simplex : Simplex) = {
    for (ridge <- simplex.getRidges()) {
      adjacentByTriangle.getOrElseUpdate(ridge, collection.mutable.Set()) += simplex
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
      case s : Simplex => s.getPosition_(new InfiniteVector(s.vertices.head.dimensions)) > EPSILON
    })
  }
  def getUpperConvexHull(simplices : collection.mutable.Set[Simplex]) : collection.mutable.Set[Simplex] = {
    simplices.filter({
      case s : Simplex => s.getPosition_(new InfiniteVector(s.vertices.head.dimensions)) < - EPSILON
    })
  }

  /**this implementation should be qhull-based*/
  def makeTesselation(points : Seq[GeometryVector]) : Unit = {
    val startSimplices = prepareStartSimplex(points.distinct)
    for (startSimplex <- startSimplices) {
      addSimplex(startSimplex)
      addNeighbours(startSimplex)
    }
    println("starting simplex received")
    var unprocessedPoints = points.distinct.toSet
    for (simplex <- simplices) {
      for (point <- unprocessedPoints) {
        if (point.isAbove(simplex)) {
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
      var visitedSimplices = collection.mutable.Set[Simplex](simplex)
      var neighbourSet = collection.mutable.Set[Simplex](simplex)
      while (!neighbourSet.isEmpty){
        val s = neighbourSet.head
        neighbourSet.remove(s)
        if (!visitedSimplices.contains(s)) {
          visitedSimplices.add(s)
          for (neighbour <- getNeighbours(s)) {
            if (!visitedSimplices.contains(neighbour)){
              if (point.isAbove(neighbour)) {
                visibleSet += neighbour
                neighbourSet.add(neighbour)
              }
            }
          }
        }
      }


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
          if (point.isAbove(newSimplex)) {
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
    //simplices.foreach(println)
    //println("all neighbours after processing")
    //adjacentByTriangle.foreach(println)
    println("end of processing")
    val s1 = getLowerConvexHull(simplices)
    val s2 = getUpperConvexHull(simplices)
    println(" " + s1.size + " " + s2.size + " " + simplices.size)
    simplices = getLowerConvexHull(simplices)
  }

}
