package benchmarker

import ru.biocad.ig.common.structures.geometry.{
    Vector, Vector2d, Vector3d,GeometryVector,
    Simplex, Tetrahedra, PointPosition
  }

import ru.biocad.ig.common.algorithms.geometry.{
    ManifoldUtils,
    DelaunayTesselation,
    DelaunayTesselationFaster,
    DelaunayTesselation3
  }
/*, DelaunayTesselationFaster*/
import ru.biocad.ig.common.io.pdb.{PDBStructure, PDBAtomInfo}

import java.util.concurrent.TimeUnit.NANOSECONDS

import java.io.{PrintWriter, File}

object Ut {
def time[R](block: => R, writer : PrintWriter, block_name : String = ""): R = {
    println("called: ")
    println(block_name)
    writer.write(block_name)
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + NANOSECONDS.toSeconds((t1 - t0)) + " seconds")
    writer.write(":\n\t" + NANOSECONDS.toSeconds((t1 - t0)) + " seconds\n ")
    result
}

def init() : (Seq[GeometryVector], Seq[GeometryVector]) = {
  val structure : PDBStructure = new PDBStructure()
  structure.readFile("2OSL.pdb")
  val v1 = structure.parse_array.collect(
    {
      case xx:PDBAtomInfo if xx.chainID == 'L' => Vector3d(xx.x, xx.y, xx.z)
    }
    ).toList
  println(v1)
  val v2 = structure.parse_array.collect({
    case xx:PDBAtomInfo if xx.chainID == 'H' => Vector3d(xx.x, xx.y, xx.z)
    }).toList
  (v1, v2)
}

def compare_timing_0(points : Seq[GeometryVector]) : Set[Simplex]= {
  var res = new DelaunayTesselation()
  res.makeTesselation(points)
  res.simplices
}

def compare_timing_1(points : Seq[GeometryVector]) = {
  var res = new DelaunayTesselation3()
  res.makeTesselation(points)
  res.simplices
}

def validate_data(points : Seq[GeometryVector], tetrahedras : Seq[Simplex]) : Boolean = {
  println("input parameters: " + points.size + " " + tetrahedras.size)
  //1. test if all points are in some triangle
  /*
  val validation_result1 = points.foldLeft((0,false)) {
    case ((orphans_counter, delaunay_false), point) =>
    {
      val bordered_set = tetrahedras.filter(t => t.getPosition(point) != PointPosition.LaysOutside)
      val points_set_size = bordered_set.filter(t => t.hasVertex(point)).size
      println("validation step: " + bordered_set.size + " " + points_set_size)

      if (bordered_set.size > points_set_size) {
        println("error! achtung! "+point.toString)
        bordered_set.filter(t => t.getPosition(point) == PointPosition.LaysInside).foreach(
          t => {
            println(t.toString + " :  " + t.getPosition(point));
            })
        return false
      }
      (orphans_counter, (points_set_size != 0))

    }
  }*/
  val validation_result = points.foldLeft(0){
    case (delaunay_false, point) =>
    {
      val bordered_set = tetrahedras.filter(t => t.getPosition(point) != PointPosition.LaysOutside)
      val points_set_size = bordered_set.filter(t => t.hasVertex(point)).size
      println("validation step: " + bordered_set.size + " " + points_set_size + " " + point)

      if (bordered_set.size > points_set_size) {
        println("error! achtung! "+point.toString)
        bordered_set.filter(t => t.getPosition(point) == PointPosition.LaysInside).foreach(
          t => {

            println(t.toString + " :  " + t.getPosition(point) + " " + point + " dist: " + t.getPosition_(point));
            })
        return false
      }
      delaunay_false + (bordered_set.size - points_set_size)
    }

  }
  /*
  val validation_result = points.foldLeft((false, 0)){
    case ((orphans_counter, delaunay_false), point) =>
    {
      val bordered_set = tetrahedras.filter(t => t.getPosition(point) != PointPosition.LaysOutside)
      val points_set_size = bordered_set.filter(t => t.hasVertex(point)).size
      println("validation step: " + bordered_set.size + " " + points_set_size)

      if (bordered_set.size > points_set_size) {
        println("error! achtung! "+point.toString)
        bordered_set.filter(t => t.getPosition(point) == PointPosition.LaysInside).foreach(
          t => {
            println(t.toString + " :  " + t.getPosition(point));
            })
        return false
      }

      (
        orphans_counter && (points_set_size != 0),
        delaunay_false + (bordered_set.size - points_set_size)
        )
    }

  }*/
  println(validation_result)
  validation_result == 0
}

def compare_timing_2(points : Seq[GeometryVector], points2 : Seq[GeometryVector]) = {
  ManifoldUtils.getDiscreteHausdorffDistance(points, points2)
}

def call_all(n : Int, logger_file_name : String = "triangulation_n_.txt") = {
  println(logger_file_name)
  val writer = new PrintWriter(new File(logger_file_name))

  val (pointsL, pointsH) = time(init(), writer)
  writer.write("processing " + pointsL.size + " and " + pointsH.size +"points\n")

  println("got: ", pointsL.size, pointsH.size)
  println("start to process 1st set of n points")

  writer.write("processing 1st set of points\n")
  val simplices1 = time(compare_timing_1(pointsL.take(n)), writer, "delaunay triangulation")
  writer.write("amount : " + simplices1.size + "\n")
  simplices1.foreach(x=>writer.write(x.toString+"\n"))
  println("valid: " + validate_data(pointsL.take(n), simplices1.toSeq))
  //println("start to process 2nd set of n points " + simplices1.size)
  //simplices1.foreach(s=> writer.write(s.toString + "\n"))
//simplices1.take(400).foreach(x=>writer.write(x.toString+"\n"))
  /*writer.write("processing 2nd set of points\n")

  val simplices2 = time(compare_timing_1(pointsH.take(n)), writer, "delaunay triangulation")
  println("simplices obtained, getting distance " + simplices2.size)
  writer.write("amount : " + simplices2.size + "\n")

  writer.write("getting Hausdorff distance\n")
  val distance = time(ManifoldUtils.getHausdorffDistance(simplices1, simplices2), writer, "getHausdorffDistance")
  println("having distance")
  println(distance)
  writer.write(distance.toString)

  writer.close()
  println("finished")
  */
  writer.close()
  //println(time(compare_timing_1(pointsL.take(n), pointsH.take(n))))
  //println(time(compare_timing_1(pointsH.take(n), pointsL.take(n))))
}

def call_all2(n : Int, logger_file_name : String = "triangulation_n_.txt") = {
  println(logger_file_name)
  val writer = new PrintWriter(new File(logger_file_name))

  val (pointsL, pointsH) = time(init(), writer)
  writer.write("processing " + pointsL.size + " and " + pointsH.size +"points\n")

  println("got: ", pointsL.size, pointsH.size)
  println("start to process 1st set of n points")
  writer.write("processing 1st set of points\n")
  val simplices1 = time(compare_timing_1(pointsL), writer, "delaunay triangulation")
  println("start to process 2nd set of n points " + simplices1.size)
  writer.write("processing 2nd set of points\n")

  val simplices2 = time(compare_timing_1(pointsH), writer, "delaunay triangulation")
  println("simplices obtained, getting distance " + simplices2.size)
  writer.write("getting Hausdorff distance\n")

  val distance = time(ManifoldUtils.getHausdorffDistance(simplices1.toSeq, simplices2.toSeq), writer, "getHausdorffDistance")
  println("having distance")
  println(distance)
  writer.write(distance.toString)
  writer.close()
  println("finished")
  //println(time(compare_timing_1(pointsL.take(n), pointsH.take(n))))
  //println(time(compare_timing_1(pointsH.take(n), pointsL.take(n))))
}

}
