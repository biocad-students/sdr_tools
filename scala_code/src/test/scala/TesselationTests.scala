import org.scalatest._
import Inspectors._
import Matchers._

import ru.biocad.ig.common.io.pdb.{PDBStructure, PDBAtomInfo}

import ru.biocad.ig.common.structures.geometry._

import ru.biocad.ig.common.algorithms.geometry._

import java.util.concurrent.TimeUnit.NANOSECONDS

import scala.io.Source

class TesselationTests extends FlatSpec with Matchers {
  def time[R](block: => R, block_name : String = "") : R = {
      println("called method: " + block_name)
      val t0 = System.nanoTime()
      val result = block
      val t1 = System.nanoTime()
      println("Elapsed time: " + NANOSECONDS.toSeconds((t1 - t0)) + " seconds")
      result
  }

  def loadPDB(filename : String, chains : Seq[Char] = Seq('L', 'H')) : Seq[Seq[GeometryVector]] = {
    val structure : PDBStructure = new PDBStructure()
    structure.readFile(filename)
    chains.map({chainName =>  {
      structure.parse_array.collect({
          case xx : PDBAtomInfo if xx.chainID == chainName => Vector3d(xx.x, xx.y, xx.z)
        }).toList
    }})
  }

  def testTriangulation(points : Seq[GeometryVector], n : Int) = {
    println("start to process set of %d points".format(n))
    val simplices1 = time(compare_timing(points.take(n)), "delaunay triangulation")
  }

  def compare_timing(points : Seq[GeometryVector]) : Seq[Simplex] = {
    var res = new QHull()
    res.makeTesselation(points)
    res.simplices.toSeq
  }


  it should "do some benchmarking" in {
    assert(getClass().getResource("/2OSL.pdb") != null);

    val Seq(pointsL, pointsH) = time(loadPDB(
      getClass().getResource("/2OSL.pdb").getFile()
      ))
    println("got: ", pointsL.size, pointsH.size)

    testTriangulation(pointsL, 10)
    testTriangulation(pointsL, 25)
    testTriangulation(pointsL, 100)
    testTriangulation(pointsL, 1000)
  }

  def validate_data(points : Seq[GeometryVector], tetrahedras : Seq[Simplex]) : Unit = {
    //println(points.diff(tetrahedras.flatMap(_.vertices).distinct))
    tetrahedras.flatMap(_.vertices).distinct should contain theSameElementsAs points
    tetrahedras.size should be > 0
    tetrahedras.foreach({
      tetrahedra => points.foreach({
        point =>
        if (point.isAbove(tetrahedra))
         ((tetrahedra.getPosition(point)) should not be (PointPosition.LaysInside))
      })
    })
  }

  it should "not return isAbove== true for given vertices" in {
    val vectors = Seq(Vector3d(23.332, 52.901, -20.613),
        Vector3d(21.946, 52.459, -20.95),
        Vector3d(21.011, 53.663, -21.105),
        Vector3d(21.011, 54.571, -20.271))
    val simplex = new Simplex(vectors,
        Vector3d(21.825, 53.398500000000006, -20.734750000000002).lifted,
        Vector3d(21.825, 53.398500000000006, -20.734750000000002))
    forAll(vectors){
      v => v.isAbove(simplex) should be (false)
    }
    val p1=  Vector3d(21.011, 54.571, -20.271)
    p1.isAbove(simplex) should be (false)
    val p2 = Vector3d(23.332, 52.901, -20.613)
    p2.isAbove(simplex) should be (false)
    val vectors2 = Seq(Vector3d(21.011, 53.663,  -21.105),
        Vector3d(21.105, 52.204, -18.517),
        Vector3d(19.258, 51.212, -17.275),
        Vector3d(20.223, 53.672, -22.176))
    val simplex2 = new Simplex(vectors2,
      vectors2.map(_.lifted).reduceLeft(_ + _)/4 + new Vector(Seq(0,0,0,1))
    ).reorient()
    (  vectors2.map(_.lifted).reduceLeft(_ + _)/4 - new Vector(Seq(0,0,0,1))).isAbove(simplex2) should be (true)
    (  vectors2.map(_.lifted).reduceLeft(_ + _)/4 + new Vector(Seq(0,0,0,1))).isBelow(simplex2) should be (true)

    simplex2.getPosition(p2) should not be (PointPosition.LaysInside)
  }

  it should "be valid" in{
    assert(getClass().getResource("/2OSL.pdb") != null)

    val Seq(pointsL, pointsH) = time(loadPDB(getClass().getResource("/2OSL.pdb").getFile()))
    println("got %d points in chain L".format(pointsL.size))
    Seq(6, 10, 15, 20, 100, 200, 300, 500).foreach({
      n => {
        println("testing tiny datasets - %d points in chain".format(n))
        val triangulationResult = compare_timing(pointsL.take(n))
        validate_data(pointsL.take(n), triangulationResult.toSeq)
      }
    })
    println("testing big dataset - %d points in chain".format(pointsL.size))
    val triangulationResult = compare_timing(pointsL)
    validate_data(pointsL, triangulationResult.toSeq)

  }

  it should "prepareStartSimplex correctly" in {
    assert(getClass().getResource("/2OSL.pdb") != null)

    val Seq(pointsL, pointsH) = time(loadPDB(getClass().getResource("/2OSL.pdb").getFile()))
    var res = new QHull()
    val simplices = res.prepareStartSimplex(pointsL.take(15))
    forAll(simplices) {
      simplex => {
        val meanposition = simplex.vertices.map(_.lifted).reduceLeft(_ + _)/simplex.vertices.size
        assert(simplex.lowerPoint.dimensions == meanposition.dimensions)
        val testPoint = meanposition -(simplex.lowerPoint - meanposition)
        //println(simplex.getDistance(testPoint))
        //println(simplex.getDistance(simplex.lowerPoint))
        testPoint.isAbove(simplex) should be (true)
        simplex.lowerPoint.isAbove(simplex) should be (false)
      }

    }

  }

}
