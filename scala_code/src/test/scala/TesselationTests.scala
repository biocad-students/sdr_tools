import org.scalatest.{Matchers, FlatSpec}

import ru.biocad.ig.common.io.pdb.{PDBStructure, PDBAtomInfo}

import ru.biocad.ig.common.structures.geometry._

import ru.biocad.ig.common.algorithms.geometry._

import java.util.concurrent.TimeUnit.NANOSECONDS

import scala.io.Source
/**
class TesselationTests extends FlatSpec with Matchers {
  def time[R](block: => R, block_name : String = ""): R = {
      println("called method: " + block_name)
      val t0 = System.nanoTime()
      val result = block
      val t1 = System.nanoTime()
      println("Elapsed time: " + NANOSECONDS.toSeconds((t1 - t0)) + " seconds")
      result
  }

  def loadPDB(filename : String) : (Seq[GeometryVector], Seq[GeometryVector]) = {
    val structure : PDBStructure = new PDBStructure()
    structure.readFile(filename)
    val v1 = structure.parse_array.collect(
      {
        case xx:PDBAtomInfo if xx.chainID == 'L' => Vector3d(xx.x, xx.y, xx.z)
      }
      ).toList
    val v2 = structure.parse_array.collect({
      case xx:PDBAtomInfo if xx.chainID == 'H' => Vector3d(xx.x, xx.y, xx.z)
      }).toList
    (v1, v2)
  }

  def testTriangulation(pointsL : Seq[GeometryVector],
      pointsH : Seq[GeometryVector], n : Int, m : Int) = {
    println("start to process 1st set of n points")
    val simplices1 = time(compare_timing_1(pointsL.take(n)), "delaunay triangulation")
    println("start to process 2nd set of m points")
    val simplices2 = time(compare_timing_1(pointsL.take(m)), "delaunay triangulation")
    println("simplices obtained, getting distance")
    println(
      time(ManifoldUtils.getHausdorffDistance(simplices1, simplices2), "getHausdorffDistance")
    )
    println("finished")
  }

  def compare_timing_1(points : Seq[GeometryVector]) : Seq[Simplex] = {
    var res = new DelaunayTesselation()
    res.makeTesselation(points)
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
    println("testing 2 ")
    println(
      tesselation.appendPoint(tetrahedras, Vector3d(100, 100, 100))
    )
  }


  it should "do some benchmarking" in {
    assert(getClass().getResource("/2OSL.pdb") != null);

    val (pointsL, pointsH) = time(loadPDB(
      getClass().getResource("/2OSL.pdb").getFile()
      ))
    println("got: ", pointsL.size, pointsH.size)

    println("testing tiny dataset - 10 points per chain")
    testTriangulation(pointsL, pointsH, 10, 10)
    println("testing bigger dataset - 25 points per chain")
    testTriangulation(pointsL, pointsH, 25, 25)
    println("testing dataset - 30 points and 10 points")
    testTriangulation(pointsL, pointsH, 30, 10)
    println("testing dataset - 30 points and 10 points")
    testTriangulation(pointsL, pointsH, 10, 30)
  }
}*/
