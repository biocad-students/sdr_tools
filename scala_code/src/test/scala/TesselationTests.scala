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
        val triangulationResult = compare_timing_1(pointsL.take(n))
        validate_data(pointsL.take(n), triangulationResult.toSeq)
      }
    })
    println("testing big dataset - %d points in chain".format(pointsL.size))
    val triangulationResult = compare_timing_1(pointsL)
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

  it should "be valid2" in {
    val point = Vector3d(21.946, 52.459, -20.95)
    val simplex = new Simplex(Seq(
Vector3d(21.405, 51.524, -19.856),
Vector3d(21.011, 54.571, -20.271),
Vector3d(23.332, 52.901, -20.613),
Vector3d(21.105, 52.204, -18.517))//,
 //new Vector(Seq(23.332, 52.901, -20.613, 3767.793794)),
//Vector3d(21.71325, 52.800000000000004, -19.81425)
)

point.isAbove(simplex.reorient()) should be (true)
point.isAbove(simplex) should be (true)

  }
}
