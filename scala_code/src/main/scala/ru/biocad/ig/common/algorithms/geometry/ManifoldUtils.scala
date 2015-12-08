package ru.biocad.ig.common.algorithms.geometry

import ru.biocad.ig.common.structures.geometry.{
  Simplex,
  GeometryVector,
  InfiniteVector, PointPosition
}
import scala.math.sqrt

object ManifoldUtils {

  def getRMSD(a : Seq[GeometryVector], b : Seq[GeometryVector]) : Double = {
    sqrt((a, b).zipped.map(_ - _).map(_.lengthSquared).reduceLeft(_ + _)/a.length)
  }
  //
  def getCofactorsVector(lines : Seq[Seq[Double]], multiplier : Double = 1) : Seq[Double] = lines match {
    case Seq() => Seq(0)
    case Seq(Seq(x, y)) => Seq(y * multiplier, - x * multiplier)
    case _ => {
      val indicesAndSignatures = lines.head.scanLeft((0, 1)) {
        case ((index, permutationSignature), _) => (index + 1, - permutationSignature)
      }.init
      indicesAndSignatures.map({
        case (index, sign) => {
          val determinant = lines.map(
            _.zipWithIndex.collect(
              {case (cell, cellColumnIndex) if cellColumnIndex != index => cell}
            )
          )
          val multipliers = getCofactorsVector(determinant.tail, multiplier * sign)
          (determinant.head, multipliers).zipped.map( _ * _ ).reduceLeft( _ + _ )
        }
      })
    }
  }

  /**Input matrix describes first (n-1) lines of n*n matrix,
  * result is a sequence of cofactors for determinant of n*n matrix, computed along last (now unknown) row
  * so, to find n*n matrix determinant, simply call:
  * {{{
  *  val cofactorsFunc = getCofactors(...)
  *  val determinant = cofactorsFunc(lastLine)
  * }}}
  * - this gives a determinant value for matrix with last line described in lastLine sequence of values
  * P.S. - if someone could change this implementation to tail-recursion or trampoline call, i'm gonna buy him a beer or coffee (or icecream).
  */
  def getCofactors(matrix : Seq[Seq[Double]]) : Seq[Double] => Double = {
    val cofactors = getCofactorsVector(matrix, 1)
    assert(matrix.length == matrix.head.length - 1)
    (cofactors, _ : Seq[Double]).zipped.map( _ * _ ).reduceLeft( _ + _ )
  }

  def getDeterminant(matrix : Seq[Seq[Double]]): Double = getCofactors(matrix.tail)(matrix.head)

  /**returns result of multiplication between matrix and matrix^{T}
  */
  def quadMatrix(matrix : Seq[Seq[Double]]) : Seq[Seq[Double]] = {
    matrix.map({
      case line : Seq[Double] => matrix.map((line, _).zipped.map(_ * _).reduceLeft(_ + _))
    })
  }

  def normalVectors(points : Seq[Seq[Double]]) : Seq[Seq[Seq[Double]]] = {
    (0 to points.head.size - 1).map({ i =>
      points.map(_.zipWithIndex.map({ case (a, index) => if (index == i) 1 else a }))
    })
  }

  def normalize(point : Seq[Double]) : Double = sqrt((point, point).zipped.map(_ * _).reduceLeft(_ + _))

  def getNormals(points : Seq[Seq[Double]]) : Seq[Double] = normalVectors(points).map(getDeterminant(_))

  def getSimplexNormalEquation(points : Seq[GeometryVector]) : Seq[Double] => Double = {
    val n = getNormals(points.map(_.lifted.coordinates))
    val nLength = normalize(n)
    val d = getDeterminant(points.map(_.lifted.coordinates))
    (v : Seq[Double]) => {
      assert(v.size == points.head.lifted.dimensions)
      ((n, v).zipped.map(_ * _ ).reduceLeft( _ + _ ) - d) / nLength
    }
  }

  def getSimplexNormalEquationParameters(points : Seq[GeometryVector], epsilon : Double) : (Seq[Double], Double, GeometryVector => Double) = {
    val n = getNormals(points.map(_.lifted.coordinates))
    val nLength = normalize(n)
    if (nLength.abs < epsilon) {
      val d = getDeterminant(points.map(_.lifted.coordinates))
      return (n, d, (v : GeometryVector) => {
        assert(v.dimensions == points.head.dimensions + 1)
        ((n, v.coordinates).zipped.map(_ * _).reduceLeft(_ + _) - d)
      })
    }

    val nNormalized = n.map(_ / nLength)//todo: add zero check
    val d = getDeterminant(points.map(_.lifted.coordinates)) / nLength
    (nNormalized, d, (v : GeometryVector) => {
      assert(v.dimensions == points.head.dimensions + 1)
      ((nNormalized, v.coordinates).zipped.map(_ * _).reduceLeft(_ + _) - d)
    })
  }

  def updateSimplices(s : Simplex, v : GeometryVector, epsilon : Double) : Seq[Simplex] = {
    if (s.isFlat) {
      println("in updateSimplices with flat simplex")
    }
    if (s.getPosition(v) == PointPosition.LaysOutside) {

      val (p, tr) = s.iterateThroughVertices().toSeq.filter({case (p, triangle) =>
        val testFunc = ManifoldUtils.getCofactors((triangle ++ Seq(InfiniteVector(4))).map(_.toSeq))
        testFunc(v.toSeq)*testFunc(p.toSeq) < epsilon
      }).head
      val temp = new Simplex(tr :+ v)
      if (temp.getPosition(p) == PointPosition.LaysOutside)
        return Seq(s, temp)
    }

    updateSimplices(Seq(s), v, epsilon)

    /*s.iterateThroughVertices().filter(x => {
      val testFunc = ManifoldUtils.getCofactors((x._2 ++ Seq(InfiniteVector(3))).map(_.toSeq) )
      testFunc(v.toSeq)* testFunc(x._1.toSeq) > 0.0
    } ).map(x => new Simplex(x._2 ++ Seq(v)) ).toSet.toSeq*/
  }

  def updateSimplices(simplices : Seq[Simplex], v : GeometryVector, epsilon : Double) : Seq[Simplex] = {
    simplices.find(_.isFlat) match {
      case Some(_) => println("in updateSimplices with flat simplex")
      case None => ()
    } //TODO: this is for debugging purposes, to remove later

    //println("updateSimplices: "+ss.size)
    val all_vertices  = simplices.flatMap(_.vertices).toSet.toSeq
    val all_triangles = simplices.flatMap(_.getTriangles())
    val unique_triangles = all_triangles.toSet.toSeq.diff(all_triangles.diff(all_triangles.toSet.toSeq).toSet.toSeq)
    //println( "unprocessed: "+ss.size+" all " + all_triangles.size + " , unique: " + unique_triangles.size)
    unique_triangles.filter(triangle => {
      val testFunc = ManifoldUtils.getCofactors((triangle.toSeq ++ Seq(InfiniteVector(3))).map(_.toSeq))
      val testFunc2 : Seq[Double] => Double = testFunc(_) * testFunc(v.toSeq)
      all_vertices.filter(vert => testFunc(vert.toSeq).abs > epsilon).forall({
        case vertex => testFunc2(vertex.toSeq) > 0.0
      })
    }).map(x => new Simplex(x.toSeq ++ Seq(v))).toSet.toSeq
  }

/*
  def updateSimplices(s : Simplex, v : GeometryVector) : Seq[Simplex] = {
    val pointPosition = s.getPosition(v)
    if (pointPosition == PointPosition.LaysOutside) {

    }
    val positioningResult = s.iterateThroughVertices().filter(x => {
      val testFunc = ManifoldUtils.getCofactors((x._2 ++ Seq(InfiniteVector(3))).map(_.toSeq) )
      testFunc(v.toSeq)* testFunc(x._1.toSeq) > 0.0
    } )
    if (positioningResult.size == 4) {
      return s.getTriangles().map(triangle => {
        new Simplex(triangle.toSeq ++ Seq(v))
      })
    }
    //.map(x => new Simplex(x._2 ++ Seq(v)) ).toSet.toSeq
  }
*/

  /**for given pair of vector, compute: 1. projection of 1st to 2nd; 2. orthogonal 1st part */
  def getProjections(v1 : GeometryVector, v2 : GeometryVector) = {
    val collinear = v2*((v1*v2)/(v2*v2))
    (v1 - collinear, collinear)
  }

  def getDistance(triangle : Set[GeometryVector], p4: GeometryVector) : Double = triangle.toSeq match {
    case Seq(p1, p2, p3) => getDistance((p1, p2, p3), p4) : Double
  }

  def getDistance(triangle : (GeometryVector, GeometryVector, GeometryVector), p4: GeometryVector) : Double = {
    val (p1, p2, p3) = triangle
    val (ortho, collinear) = getProjections(p2-p1, p3-p1)
    val (o1, c1) = getProjections(p4 - p1, ortho)
    val (o2, c2) = getProjections(o1, collinear)
    o1.length
  }

  def getDistance(s : Simplex, p : GeometryVector) : Double = s.getPosition(p) match {
    case PointPosition.LaysOutside => s.getTriangles().map(getDistance(_, p)).min
    case _ => - s.getTriangles().map(getDistance(_, p)).max
  }

  /** finds 2 nearest points for 2 line segments, defined by pairs of points p1, p2 and p3, p4 */
  def getNearestPoints(
      p1 : GeometryVector,
      p2 : GeometryVector,
      p3 : GeometryVector,
      p4 : GeometryVector
  ) : (GeometryVector, GeometryVector) = {
    def getPoint(p1 : GeometryVector, p2 : GeometryVector, k : Double) : GeometryVector = k match {
      case k if k <= 0 => p1
      case k if k >= 1 => p2
      case _ => p1 + (p2 - p1) * k
    }

    val p4_3 = p4 - p3
    val p2_1 = p2 - p1
    val p1_3 = p1 - p3
    val l1 = p2_1 * p2_1
    val l2 = p4_3 * p4_3
    val l12 = p2_1 * p4_3

    val lineVectorsAreOrthogonal = l12 == 0 // FIX: this can be very small value due to computation inaccuracy
    if (lineVectorsAreOrthogonal) {
      //val k1 = ((p3 - p1) * (p2 - p1)) / ((p2 - p1) * (p2 - p1))
      //if (k1 > 0 && k1 < 1)
      val k1 = - (p1_3 * p2_1) / l1
      val k2 = (p1_3 * p4_3) / l2
      val point1 = getPoint(p1, p2, k1)
      val point2 = getPoint(p3, p4, k2)
      (point1, point2)
    }
    else {
      val linesAreParallel = (l2 * l1 - l12 * l12) == 0 // FIX: this can also be very small value
      if (linesAreParallel) {
        val k1 = - (p1_3 * p2_1) / l1 // p3
        val k11 = - ((p1 - p4) * p2_1) / l1 // p4
        val k2 = - ((p3 - p1) * p4_3) / l2 // p1
        val k22 = -((p3 - p2) * p4_3) / l2 // p2
        val pp3 = getPoint(p1, p2, k1)
        val pp4 = getPoint(p1, p2, k11)
        val pp1 = getPoint(p3, p4, k2)
        val pp2 = getPoint(p3, p4, k22)
        Seq((p1, pp1), (p2, pp2), (pp3, p3), (pp4, p4)).minBy(x => x._1.distanceTo(x._2))
      }
      else {
        val k1 = - (l1 * (p1_3 * p4_3) - l12 * (p1_3 * p2_1)) / (l1 * l2 - l12 * l12)
        val k2 = (l2 * (p1_3 * p2_1) - l12 * (p1_3 * p4_3)) / (l2 * l1 - l12 * l12)
        val point1 = getPoint(p1, p2, k1)
        val point2 = getPoint(p3, p4, k2)
        (point1, point2)
      }
    }
  }
}
