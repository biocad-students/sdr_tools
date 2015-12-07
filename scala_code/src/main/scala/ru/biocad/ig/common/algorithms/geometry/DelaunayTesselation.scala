package ru.biocad.ig.common.algorithms.geometry

import ru.biocad.ig.common.structures.geometry._

/*this is slowest implementation possible*/
class DelaunayTesselation {
  val EPSILON = 0.001

  var simplices : Set[Simplex] = Set()

  def prepareBoundingSimplex(points : Seq[GeometryVector]) : Simplex = {
    new Simplex(points.take(4))
  }

  /** adds point to existing tesselation
    *
    */
  def appendPoint(simplices : Seq[Simplex], new_point : GeometryVector) : Seq[Simplex] /** : (Seq[Simplex], Seq[Simplex]) */ = {
    val p = simplices.partition(_.getPosition(new_point) == PointPosition.LaysOutside)
    if (p._2.size >0)
      return (p._1 ++ /** don't modify */
          ManifoldUtils.updateSimplices(p._2, new_point, EPSILON)).toSet.toSeq /** this gets modified */
    //was:
    ////val nearest = findNearestSimplex(new_point, simplices)
    //val rad = nearest.vertices.filter(_ != nearest.vertices.maxBy(new_point.distanceTo(_))).maxBy(new_point.distanceTo(_)).distanceTo(new_point)

    //println(nearest)
    //println("----")
    //val all_nearest : Seq[Simplex] = simplices.filter(s => s.vertices.map(new_point.distanceTo(_)).min <= rad)
    //println(all_nearest)
    //println(simplices.diff(all_nearest))
    val all_triangles = simplices.flatMap(_.getTriangles())
    val unique_triangles = all_triangles.toSet.toSeq.diff(all_triangles.diff(all_triangles.toSet.toSeq).toSet.toSeq)

    val borderLine = simplices.filter(s=>
      unique_triangles.contains(findDividingFacet(s, new_point))
    )

    //(simplices.diff(all_nearest)) ++ all_nearest.flatMap(x=>ManifoldUtils.updateSimplices(x, new_point))
    (simplices.diff(borderLine)) ++ borderLine.flatMap(ManifoldUtils.updateSimplices(_, new_point, EPSILON))
  }

  def findNearestSimplex(new_point : GeometryVector, start_simplices : Seq[Simplex]) : Simplex = {
    start_simplices.minBy(ManifoldUtils.getDistance(_, new_point))
  }

  def findDividingFacet(simplex : Simplex, p : GeometryVector):Set[GeometryVector] = {
    for ((v, Seq(x1, x2, x3)) <- simplex.iterateThroughVertices()) {
      if (ManifoldUtils.getDistance((x1, x2, x3), p) < v.distanceTo(p))
        return  Set(x1, x2, x3)
    }
    return Set()
  }



  def removeBoundingSimplex(tesselationResult : Seq[Simplex], startSimplex : Simplex) = {
    tesselationResult./*filterNot(
      simplex =>
        startSimplex.vertices.foldLeft(false) {
          case (result, p) => result || simplex.hasVertex(p)
        }
    ).*/toSet
  }

  def makeTesselation(points : Seq[GeometryVector]) : Seq[Simplex] = {
    val startSimplex = prepareBoundingSimplex(points)
    var l = 0
    val tesselationResult = points.drop(4).foldLeft(Seq(startSimplex)) {
      case (simplices, point) => {
        l+=1;
        println("processing point no: " + l);
        appendPoint(simplices, point)
        }
    }
    simplices = removeBoundingSimplex(tesselationResult, startSimplex)
    simplices.toSeq
  }
}
