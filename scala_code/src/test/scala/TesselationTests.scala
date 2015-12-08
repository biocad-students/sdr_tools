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

  def testTriangulation(pointsL : Seq[GeometryVector],
      pointsH : Seq[GeometryVector], n : Int, m : Int) = {
    println("start to process 1st set of n points")
    val simplices1 = time(compare_timing_0(pointsL.take(n)), "delaunay triangulation")
    println("start to process 2nd set of m points")
    val simplices2 = time(compare_timing_1(pointsL.take(n)), "delaunay triangulation")
    println("finished")
  }

  def compare_timing_0(points : Seq[GeometryVector]) : Seq[Simplex] = {
    var res = new DelaunayTesselation()
    res.makeTesselation(points)
    res.simplices.toSeq
  }
  def compare_timing_1(points : Seq[GeometryVector]) : Seq[Simplex] = {
    var res = new QHull()
    res.makeTesselation(points)
    res.simplices.toSeq
  }

  it should "create initial bounding simplex correctly"

  it should "appendPoint to existing tesselation correctly" in {
    val tesselation = new DelaunayTesselation()

    var tetrahedras = Seq(new Tetrahedra(
      Vector3d(0, 0, 0),
      Vector3d(0, 0, 100),
      Vector3d(100, 0, 0),
      Vector3d(0, 100, 0)
    ))

    tesselation.appendPoint(tetrahedras, Vector3d(10, 10, 10))
    println("testing 2")
    println(
      tesselation.appendPoint(tetrahedras, Vector3d(100, 100, 100))
    )
  }


  it should "do some benchmarking" in {
    assert(getClass().getResource("/2OSL.pdb") != null);

    val Seq(pointsL, pointsH) = time(loadPDB(
      getClass().getResource("/2OSL.pdb").getFile()
      ))
    println("got: ", pointsL.size, pointsH.size)

    println("testing tiny dataset 10 points per chain")
    testTriangulation(pointsL, pointsH, 10, 10)
    println("testing bigger dataset - 25 points per chain")
    testTriangulation(pointsL, pointsH, 25, 25)
    println("testing dataset - 30 points and 10 points")
    testTriangulation(pointsL, pointsH, 30, 10)
    println("testing dataset - 30 points and 10 points")
    testTriangulation(pointsL, pointsH, 10, 30)
  }

  def validate_data(points : Seq[GeometryVector], tetrahedras : Seq[Simplex]) : Unit = {
    tetrahedras.flatMap(_.vertices).distinct should contain theSameElementsAs points

    forAll(tetrahedras){
      tetrahedra => forAll(points) {
        point =>
        if (point.isAbove(tetrahedra))
         ((tetrahedra.getPosition(point)) should not be (PointPosition.LaysInside))
      }
    }
  }

  it should "not return isAbove== true for given vertices" in {
    val vectors = Seq(Vector3d(23.332, 52.901, -20.613),
        Vector3d(21.946, 52.459, -20.95),
        Vector3d(21.011, 53.663, -21.105),
        Vector3d(21.011, 54.571, -20.271))
    val simplex = new Simplex(vectors,
        Vector3d(21.825, 53.398500000000006, -20.734750000000002),
        Vector3d(21.825, 53.398500000000006, -20.734750000000002))
    forAll(vectors){
      v => v.isAbove(simplex) should be (false)
    }
    val p1=  Vector3d(21.011, 54.571, -20.271)
    p1.isAbove(simplex) should be (false)
    val p2 = Vector3d(23.332, 52.901, -20.613)
    p2.isAbove(simplex) should be (false)
    val simplex2 = new Simplex(
      Seq(Vector3d(21.011, 53.663, -21.105),
        Vector3d(21.105, 52.204, -18.517),
        Vector3d(19.258, 51.212, -17.275),
        Vector3d(20.223, 53.672, -22.176)),
      Vector3d(20.39925, 52.68775, -19.76825),
      Vector3d(20.39925, 52.68775, -19.76825)
    )
    Vector3d(105,204, -517).isAbove(simplex2) should be (true)
    simplex2.getPosition(p2) should not be (PointPosition.LaysInside)
  }

  it should "be valid" in{
    assert(getClass().getResource("/2OSL.pdb") != null)

    val Seq(pointsL, pointsH) = time(loadPDB(getClass().getResource("/2OSL.pdb").getFile()))
    println("got %d points in chain L".format(pointsL.size))
    Seq(10, 15/*, 20, 25, 30, 40, 100*/).foreach({
      n => {
        println("testing tiny datasets - %d points in chain".format(n))
        val triangulationResult = compare_timing_1(pointsL.take(n))
        validate_data(pointsL.take(n), triangulationResult.toSeq)
      }
    })
    println("testing big dataset - %d points in chain".format(pointsL.size))
    val triangulationResult = compare_timing_0(pointsL)
    //validate_data(pointsL, triangulationResult.toSeq)

  }
}
