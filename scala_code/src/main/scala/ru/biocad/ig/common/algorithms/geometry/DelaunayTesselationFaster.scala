package ru.biocad.ig.common.algorithms.geometry

import ru.biocad.ig.common.structures.geometry._


class DelaunayTesselationFaster {
  val EPSILON = 0.1

  var simplices : Set[Simplex] = Set()
  var last_simplices : Seq[Simplex]= Seq()

  var adjacentByVertex   = collection.mutable.Map[GeometryVector, collection.mutable.Set[Simplex]]()
  var adjacentByEdge     = collection.mutable.Map[Set[GeometryVector], collection.mutable.Set[Simplex]]()
  var adjacentByTriangle = collection.mutable.Map[Set[GeometryVector], collection.mutable.Set[Simplex]]()

  /** removes simplex from all hashmaps in current tesselation */
  def removeSimplex(s : Simplex) = {
/*
    println("in removeSimplex")
    println(adjacentByVertex.keys.size) //.flatMap(x=>x.toSeq).toSeq.distinct.size)
    println(adjacentByEdge.values.flatMap(x=>x.toSeq).toSeq.distinct.size)
    println(adjacentByTriangle.values.flatMap(x=>x.toSeq).toSeq.distinct.size)
*/
    s.vertices.filter(adjacentByVertex.contains(_)).foreach(adjacentByVertex(_) -= s)
    s.getLineSegments().filter(adjacentByEdge.contains(_)).foreach(adjacentByEdge(_) -= s)
    s.getTriangles().filter(adjacentByTriangle.contains(_)).foreach(adjacentByTriangle(_) -= s)
    //simplices = simplices.filterNot(_!=s)
    /*
    println("in removeSimplex - after removing")
    println(adjacentByVertex.keys.size)//values.flatMap(x=>x.toSeq).toSeq.distinct.size)
    println(adjacentByEdge.values.flatMap(x=>x.toSeq).toSeq.distinct.size)
    println(adjacentByTriangle.values.flatMap(x=>x.toSeq).toSeq.distinct.size)
    println("exiting")*/
  }

  /** adds simplex to all hashmaps in current tesselation */
  def addSimplex(s : Simplex) = {
    /*println("in addSimplex")
    println(adjacentByVertex.keys.size)//values.flatMap(x=>x.toSeq).toSeq.distinct.size)
    println(adjacentByEdge.values.flatMap(x=>x.toSeq).toSeq.distinct.size)
    println(adjacentByTriangle.values.flatMap(x=>x.toSeq).toSeq.distinct.size)
    */
    s.vertices.foreach(
      adjacentByVertex.getOrElseUpdate(_, collection.mutable.Set()) += s
    )
    s.getLineSegments().foreach(
          adjacentByEdge.getOrElseUpdate(_, collection.mutable.Set()) += s
    )
    s.getTriangles().foreach(
      adjacentByTriangle.getOrElseUpdate(_, collection.mutable.Set()) += s
    )
    //    simplices += s

  /*  println("in addSimplex - after adding")
    println(adjacentByVertex.keys.size)//values.flatMap(x=>x.toSeq).toSeq.distinct.size)
    println(adjacentByEdge.values.flatMap(x=>x.toSeq).toSeq.distinct.size)
    println(adjacentByTriangle.values.flatMap(x=>x.toSeq).toSeq.distinct.size)
    println("exiting")*/
  }

  def prepareBoundingSimplex(points : Seq[GeometryVector]) : Simplex = {
    new Simplex(points.take(4))
  }


  /** adds point to existing tesselation
    */
  def appendPoint(new_point : GeometryVector) = {
    //val p = simplices.partition(_.getPosition(new_point) == PointPosition.LaysOutside)
    //p._1 ++ /** don't modify */
      //    p._2.flatMap(ManifoldUtils.updateSimplices(_, new_point)) /** this gets modified */
      val s = findNearestSimplex(new_point, last_simplices)
      val neighbours = getNeighbours(s)
      val old_simplices = (s +: neighbours).filter(_.getPosition(new_point) != PointPosition.LaysOutside).toSet.toSeq
//      old_simplices.foreach(simpl => {
        //if (simpl.getPosition(new_point)!= PointPosition.LaysInside)
//        println(simpl.getPosition(new_point))
//      })
      old_simplices.foreach(removeSimplex(_))
      //println("ddd")
      //println("old size: " + old_simplices.size)
      //removeSimplex(s)
      //at this point l contains all simplices that should be modified
      if (old_simplices.size > 0) {
        val new_simplices = ManifoldUtils.updateSimplices(old_simplices, new_point)
        last_simplices = new_simplices
        new_simplices.foreach(addSimplex(_))
        //println("new size: " + new_simplices.size)
      }
      else {
        val new_simplices = ManifoldUtils.updateSimplices(s, new_point)
        last_simplices = new_simplices
        new_simplices.foreach(addSimplex(_))
      }
//new_simplices.foreach(println)
      //do smth
  }

  /**methods find seq of simplices adjacent to s*/
  def getNeighbours(s : Simplex) : Seq[Simplex] = {
    (
      s.vertices.flatMap(adjacentByVertex.getOrElse(_, Set()).toSeq) ++
      s.getLineSegments().flatMap(adjacentByEdge.getOrElse(_, Set()).toSeq) ++
      s.getTriangles().flatMap(adjacentByTriangle.getOrElse(_, Set()).toSeq)
    ).filterNot(_.equals(s)).toSet.toSeq
  }

  def findNearestSimplex(new_point : GeometryVector, start_simplices : Seq[Simplex]) : Simplex = {
    start_simplices.foreach(start_simplex => {
        if (start_simplex.getPosition(new_point) != PointPosition.LaysOutside)
          return start_simplex
      }
    )
    var start_simplex = {
      if (start_simplices.size > 0)
      start_simplices.minBy(ManifoldUtils.getDistance(_, new_point))
      else adjacentByVertex.values.flatMap(x=>x.toSeq).head
      }
    val neighbours = getNeighbours(start_simplex)
    var s = start_simplex
    if (neighbours.size==0)
      return s
    var next_simplex:Simplex = neighbours.minBy(ManifoldUtils.getDistance(_, new_point))

    var i=0
    while (!s.equals(next_simplex)) {
      s = next_simplex
      if (s.getPosition(new_point) != PointPosition.LaysOutside) {
        println("iterated: " + i.toString)
        return s
      }
      //println(s)
      val neighbours = getNeighbours(s)
      next_simplex = (s +: neighbours).minBy(ManifoldUtils.getDistance(_, new_point))

      i+=1
      if (i%100==0)
      println("i>100")
      //return next_simplex
    }
    println("iterated 5: " + i.toString)
    next_simplex
  }

def removeBoundingSimplex(tesselationResult : Seq[Simplex], startSimplex : Simplex) = {
  tesselationResult.filterNot(
    simplex =>
      startSimplex.vertices.foldLeft(false) {
        case (result, p) => result || simplex.hasVertex(p)
      }
  ).toSet.toSeq
}


  def removeBoundingSimplex(startSimplex : Simplex) = {
    //todo: should remove all simplices which are removed during this step
    //println("in removeBoundingSimplex")
    //println(startSimplex.toString)
    (
      adjacentByVertex.values.toSeq++
      adjacentByEdge.values.toSeq++
      adjacentByTriangle.values.toSeq
    ).flatMap(x=>x.toSeq)./*filter(
      simplex =>
        startSimplex.vertices.foldLeft(true) {
          case (result, p) => result && !simplex.hasVertex(p)
        }
    ).*/toSet
  }

  def makeTesselation(points : Seq[GeometryVector]) : Unit = {
    val startSimplex = prepareBoundingSimplex(points.distinct)
    addSimplex(startSimplex)
    last_simplices = Seq(startSimplex)
    var l = 0
    points.drop(4).foreach (point=>{
      l += 1
      //if (l % 2 == 0)
        println("processing: " + l)
      appendPoint(point)
    })
    println("removing")
    simplices = removeBoundingSimplex(startSimplex)
  }
}
