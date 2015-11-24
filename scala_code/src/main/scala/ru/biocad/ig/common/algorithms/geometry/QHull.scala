package ru.biocad.ig.common.algorithms.geometry

import ru.biocad.ig.common.structures.geometry._

class QHull {

  val EPSILON = 0.00001

  var simplices = collection.mutable.Set[Simplex]()
  var neighbours = collection.mutable.Map[Simplex, collection.mutable.Set[Simplex]]()
  var last_simplices : Seq[Simplex]= Seq()

  var adjacentByVertex   = collection.mutable.Map[GeometryVector, collection.mutable.Set[Simplex]]()
  var adjacentByEdge     = collection.mutable.Map[Set[GeometryVector], collection.mutable.Set[Simplex]]()
  var adjacentByTriangle = collection.mutable.Map[Set[GeometryVector], collection.mutable.Set[Simplex]]()

  var outerSet = collection.mutable.Map[Simplex, Seq[GeometryVector]]()
  //var outerSet = collection.mutable.Map[Simplex, (GeometryVector, Seq[GeometryVector])]()

  def removeSimplex(s : Simplex) = simplices.remove(s)

  def appendPoint(new_point : GeometryVector) : Unit = { }

  def addSimplex(s: Simplex) = simplices.add(s)

  def getHypervolume(pp : Seq[GeometryVector]) : Double = {
    ManifoldUtils.getDeterminant(pp.tail.map({
      line => (pp.head.lifted.coordinates, line.lifted.coordinates).zipped.map({(p0, p1) => p1 - p0})
    }))
  }

  /**
    * @return set of simplices in d+1, wich are ridges of simplex in d+2
    * shouldn't be degenerate
    */
  def prepareStartSimplex(points : Seq[GeometryVector]) : Seq[Simplex] = {
    var pp = Seq(points.maxBy(_.length), points.minBy(_.length))
    (0 to points.head.coordinates.size-1).foreach({ i=> {
      if (points.diff(pp).size > 0)
        pp = pp :+ points.diff(pp).maxBy(_.coordinates(i))
      if (points.diff(pp).size > 0)
        pp = pp :+ points.diff(pp).minBy(_.coordinates(i))
      } //FIX: i don't remember meaning of this condition
    })
    //TODO: add start simplex checks, etc.
    pp = pp.distinct.take(points.head.dimensions + 2)
    //println("got points: " + pp.size + " " + points.head.dimensions)
    if (getHypervolume(pp) > EPSILON) {
      pp = (Seq(pp.tail.head, pp.head) ++ pp.tail.tail)
    }
    val innerPoint = pp.map(_.lifted).reduceLeft(_ + _)/(pp.size)
    (new GeometryVectorIterator(pp)).map({
      case s : (GeometryVector, Seq[GeometryVector]) => new Simplex(s._2, innerPoint, s._2.reduceLeft(_ + _)/s._2.size)
    }).toSeq
  }

  /**helper method. returns furthest point for given simplex
  */
  def selectFurthest(simplex: Simplex, points : Seq[GeometryVector]) : GeometryVector = {
    points.maxBy(- simplex.getDistance(_))
    //val p2 = points.minBy(- simplex.getPosition_(_))
    //println("furthest point dist: " + simplex.getPosition_(p) +" , "+ simplex.getPosition_(p2))
  }

  def addNeighbours(simplex : Simplex) = {
    //println("in add neighbours")
    //println(simplex)
    //println("set")
    //println(adjacentByTriangle)
    simplex.getRidges().foreach({
      ridge =>
      adjacentByTriangle.getOrElseUpdate(ridge, collection.mutable.Set()) += simplex
    })
    //println(" after add neigbours")
    //println(adjacentByTriangle)
    //println("exiting from add neighbours")
  }

  def getNeighbours(simplex: Simplex) : Seq[Simplex] = {
    simplex.getRidges().flatMap(adjacentByTriangle.getOrElse(_, Seq())).toSeq.distinct.diff(Seq(simplex))
  }

  def removeNeighbours(simplex : Simplex) = {
    simplex.getRidges().filter(adjacentByTriangle.contains(_)).foreach({ triangle =>
        adjacentByTriangle(triangle) -= simplex
    })
  }

  def getBorderLine(simplices : Seq[Simplex], point : GeometryVector) : Seq[Set[GeometryVector]] = {
    //val allRidges = simplices.flatMap(_.getRidges())
    //val uniqueRidges = allRidges.toSet.toSeq.diff(allRidges.diff(allRidges.toSet.toSeq).toSet.toSeq)
    //uniqueRidges
    var horizonRidges = collection.mutable.Set[Set[GeometryVector]]()
    simplices.foreach({ simplex =>
      getNeighbours(simplex).filterNot(point.isAbove(_)).foreach({
          neighbour =>
            //println(simplex.getRidges().toSet.intersect(neigbour.getRidges().toSet))
            horizonRidges ++= simplex.getRidges().toSet.intersect(neighbour.getRidges().toSet)
      })
    })
    //println(horizonRidges.toSeq)
    horizonRidges.toSeq
  }

  def getLowerConvexHull(simplices : collection.mutable.Set[Simplex]) : collection.mutable.Set[Simplex] = {
    simplices.filter({ s => s.isInLowerConvHull() })
  }

  def getUpperConvexHull(simplices : collection.mutable.Set[Simplex]) : collection.mutable.Set[Simplex] = {
    simplices.filter({ s => (new InfiniteVector(s.vertices.head.dimensions)).isAbove(s) })
  }

  def makeCone(horizonSimplices : Seq[Simplex], point : GeometryVector) : Seq[Simplex] = {
    var ridges = getBorderLine(horizonSimplices, point)
    //println("in makeCone: ")
    //ridges.foreach(println)
    //println(point)
    var newSimplices = collection.mutable.Set[Simplex]()
    var tetrahedras = ridges.map( {case ridge => {
      (new Simplex(ridge.toSeq :+ point, horizonSimplices.head.innerPoint)).reorient()
    }})
    tetrahedras.filter(addSimplex(_)).foreach(addNeighbours(_))
    //println("tetrahedras: " + tetrahedras)
    tetrahedras
//    newSimplices
  }

  /**this implementation should be qhull-based*/
  def makeTesselation(points : Seq[GeometryVector]) : Unit = {
    val startSimplices = prepareStartSimplex(points.distinct)
    startSimplices.filter(addSimplex(_)).foreach(addNeighbours(_))

    startSimplices.foreach(println)
    println("starting simplex received")
    var unprocessedPoints = points //.distinct.toSet
    println(unprocessedPoints.size)

    simplices.scanLeft(unprocessedPoints) ({
      (unprocessedPoints, simplex) => {
        println("new simplex")
        val (abovePoints, result) = unprocessedPoints.partition(point => {
            println(simplex)
            println(point)
            println(point.isAbove(simplex) + " " + simplex.getDistance(point))
            point.isAbove(simplex)
          })
        outerSet(simplex) = outerSet.getOrElse(simplex, Seq()) ++ abovePoints
        result
      }
    })

    println(outerSet)
    //return
    println("processing outer set...")
    while (!outerSet.isEmpty) {
      var (simplex, outerPoints) = outerSet.head
      val point = selectFurthest(simplex, outerPoints.toSeq)
      var visibleSet = collection.mutable.Set[Simplex]()
      var horizonSimplices = collection.mutable.Set[Simplex]()
      var visitedSimplices = collection.mutable.Set[Simplex]()
      var neighbourSet = collection.mutable.Set[Simplex](simplex)
      //println("simplices size: " + simplices.size)
      //println(simplices.head.innerPoint)
      //println(simplices.head.innerPoint.isAbove(simplices.head))
      //println(simplices.head.getDistance(simplices.head.innerPoint))
      while (!neighbourSet.isEmpty){
        val s = neighbourSet.head
        neighbourSet.remove(s)
        //println(123)
        //println(s.getDistance(point))
        if (point.isAbove(s))
            visibleSet += s
        if (!visitedSimplices.contains(s)) {
          visitedSimplices.add(s)
          val (above, horizon) = getNeighbours(s).partition(point.isAbove(_))
          above.filterNot(visitedSimplices.contains(_)).foreach(neighbourSet.add(_))
          horizonSimplices ++= horizon
          /*foreach({ neighbour => {
            if (point.isAbove(neighbour)) {
              if (!visitedSimplices.contains(neighbour)){
                //visibleSet += neighbour
                neighbourSet.add(neighbour)
              }
            } else {
              horizonSimplices += s
            }
          }
        })*/
        }
      }

      val newSimplices = makeCone(horizonSimplices.toSeq, point)
      //TODO: add links to newly created simplices from their neighbours
      //TODO: build outside points set for all visible Set once again
      var unprocessedPoints = visibleSet.flatMap(outerSet.getOrElse(_, Seq()))
      /**
      for (simplex <- visibleSet) {
        if (outerSet.contains(simplex))
        unprocessedPoints ++= outerSet(simplex)
      }*/

      visibleSet.foreach({simplex=> {
        removeSimplex(simplex)
        outerSet.remove(simplex)
        removeNeighbours(simplex)
        }
      })
      for (newSimplex <- newSimplices) {
        /**
        unprocessedPoints = unprocessedPoints.filter({
          p => {
            if (point.isAbove(newSimplex)) {
              outerSet(newSimplex) = outerSet.getOrElseUpdate(newSimplex, Seq[GeometryVector]()) :+ p
              false
            }
            else {
              true
            }
          }
        })*/

        for (point <- unprocessedPoints) {
          if (point.isAbove(newSimplex)) {
            outerSet(newSimplex) = outerSet.getOrElseUpdate(newSimplex, Seq[GeometryVector]()) :+ point
            unprocessedPoints -= point
          }
        }

        addSimplex(newSimplex)
        addNeighbours(newSimplex)
      }

    }
    //println("all simplices after processing")
    //simplices.foreach(println)
    //println("all neighbours after processing")
    //adjacentByTriangle.foreach(println)
    //println("end of processing")
    val s1 = getLowerConvexHull(simplices)
    val s2 = getUpperConvexHull(simplices)
    println(" convex hulls: " + s1.size + " " + s2.size + " " + simplices.size)
    simplices = getLowerConvexHull(simplices)
  }

}
