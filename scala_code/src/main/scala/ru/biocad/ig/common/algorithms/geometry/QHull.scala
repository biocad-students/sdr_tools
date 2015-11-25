package ru.biocad.ig.common.algorithms.geometry

import ru.biocad.ig.common.structures.geometry._

import com.typesafe.scalalogging.slf4j.LazyLogging

class QHull(val dimensions : Int = 3) extends LazyLogging {

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
    pp = pp.distinct.take(dimensions + 2)
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
    logger.debug("in add neighbours")
    logger.debug(simplex.toString)
    logger.debug("set")
    logger.debug(adjacentByTriangle.toString)
    simplex.getRidges().foreach({
      ridge =>
      adjacentByTriangle.getOrElseUpdate(ridge, collection.mutable.Set()) += simplex
    })
    logger.debug(" after add neigbours")
    logger.debug(adjacentByTriangle.toString)
    logger.debug("exiting from add neighbours")
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
    logger.debug("in getBorderLine, ridges are:")
    logger.debug(horizonRidges.toSeq.toString)
    horizonRidges.toSeq
  }

  def getLowerConvexHull(simplices : collection.mutable.Set[Simplex]) : collection.mutable.Set[Simplex] = {
    simplices.filter({ s => s.isInLowerConvHull() })
  }

  //todo: check if simplices points are lifed, or not
  def getUpperConvexHull(simplices : collection.mutable.Set[Simplex]) : collection.mutable.Set[Simplex] = {
    simplices.filter((new InfiniteVector(dimensions)).isAbove(_))
  }

  def makeCone(horizonSimplices : Seq[Simplex], point : GeometryVector) : Seq[Simplex] = {
    var ridges = getBorderLine(horizonSimplices, point)
    logger.debug("in makeCone: ")
    logger.debug(point.toString)
    var newSimplices = collection.mutable.Set[Simplex]()
    var tetrahedras = ridges.map( {case ridge => {
      (new Simplex(ridge.toSeq :+ point, horizonSimplices.head.innerPoint)).reorient()
    }})
    tetrahedras.filter(addSimplex(_)).foreach(addNeighbours(_))
    logger.debug("tetrahedras: " + tetrahedras)
    tetrahedras
  }

  /**this implementation should be qhull-based*/
  def makeTesselation(points : Seq[GeometryVector]) : Unit = {
    val startSimplices = prepareStartSimplex(points.distinct)
    startSimplices.filter(addSimplex(_)).foreach(addNeighbours(_))

    logger.debug(startSimplices.toString)
    logger.info("starting simplex received")
    var unprocessedPoints = points //.distinct.toSet
    println(unprocessedPoints.size)

    simplices.scanLeft(unprocessedPoints) ({
      (unprocessedPoints, simplex) => {
        logger.info("new simplex")
        val (abovePoints, result) = unprocessedPoints.partition(point => {
            logger.info(simplex.toString)
            logger.info(point.toString)
            logger.info(point.isAbove(simplex).toString + " " + simplex.getDistance(point).toString)
            point.isAbove(simplex)
          })
        if (abovePoints.nonEmpty)
            outerSet(simplex) = outerSet.getOrElse(simplex, Seq()) ++ abovePoints
        result
      }
    })

    logger.info(outerSet.toString)
    //return
    logger.info("processing outer set...")
    while (!outerSet.isEmpty) {
      var (simplex, outerPoints) = outerSet.head
      val point = selectFurthest(simplex, outerPoints.toSeq)
      var visibleSet = collection.mutable.Set[Simplex]()
      var horizonSimplices = collection.mutable.Set[Simplex]()
      var visitedSimplices = collection.mutable.Set[Simplex]()
      var neighbourSet = collection.mutable.Set[Simplex](simplex)
      logger.debug("simplices size: " + simplices.size)
      logger.debug(simplices.head.innerPoint.toString)
      logger.debug(simplices.head.innerPoint.isAbove(simplices.head).toString)
      logger.debug(simplices.head.getDistance(simplices.head.innerPoint).toString)
      while (!neighbourSet.isEmpty){
        val s = neighbourSet.head
        neighbourSet.remove(s)
        logger.debug(s.getDistance(point).toString)
        if (point.isAbove(s))
            visibleSet += s
        if (!visitedSimplices.contains(s)) {
          visitedSimplices.add(s)
          val (above, horizon) = getNeighbours(s).partition(point.isAbove(_))
          above.filterNot(visitedSimplices.contains(_)).foreach(neighbourSet.add(_))
          horizonSimplices ++= horizon
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
      newSimplices.foreach({ newSimplex => {
        val (abovePoints, horizon) = unprocessedPoints.partition(p => p.isAbove(newSimplex))
        if (abovePoints.nonEmpty)
            outerSet(newSimplex) = outerSet.getOrElseUpdate(newSimplex, Seq[GeometryVector]()) ++ abovePoints
        unprocessedPoints = horizon

        addSimplex(newSimplex)
        addNeighbours(newSimplex)
      }
      })

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
