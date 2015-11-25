package benchmarker

import ru.biocad.ig.common.structures.geometry.{
    Vector, Vector2d, Vector3d,GeometryVector,
    Simplex, Tetrahedra, PointPosition
  }

import ru.biocad.ig.common.algorithms.geometry.{
    ManifoldUtils,
    DelaunayTesselation,
    DelaunayTesselationFaster,
    QHull
  }
/*, DelaunayTesselationFaster*/
import ru.biocad.ig.common.io.pdb.{PDBStructure, PDBAtomInfo}

import java.util.concurrent.TimeUnit.NANOSECONDS
import scala.io.Source
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
  val source = Source.fromURL(getClass.getResource("/2OSL.pdb"))
  structure.readFile(source)
  source.close()
  val v1 = structure.parse_array.collect(
    {
      case xx:PDBAtomInfo if xx.chainID == 'L' => Vector3d(xx.x, xx.y, xx.z)
    }
    ).toList
  //println(v1)
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
  var res = new QHull()
  res.makeTesselation(points)
  res.simplices
}

def validate_data(points : Seq[GeometryVector], tetrahedras : Seq[Simplex]) : Boolean = {
  println("input parameters: " + points.size + " " + tetrahedras.size)

  val res = points.foldLeft((0, 1)) {
    case (delaunay_false, point) =>
    {
      //FIX: check condition
      val isVertex = tetrahedras.count(t => t.hasVertex(point))
      if (isVertex == 0) {
        println("at " + delaunay_false._2 + " got orphan point " + isVertex + " "+ point)
        if (tetrahedras.count(t=> (t.getPosition(point) == PointPosition.LaysOnNSphere && !t.hasVertex(point))) > 0 )
        {
          println("point lays on n-dimensional sphere for some tetrahedras")
          //println(tetrahedras.count(t=> t.getPosition(point)==PointPosition.LaysOnNSphere))
          //println(point)
          //println(tetrahedras.filterNot(_.hasVertex(point)))
        }
        (delaunay_false._1 + 1, delaunay_false._2+1)
      } else{
      (delaunay_false._1, delaunay_false._2+1)
      }
    }
  }

  val validation_result = points.foldLeft(0){
    case (delaunay_false, point) =>
    {
      val bordered_set = tetrahedras.filter(t => t.getPosition(point) != PointPosition.LaysOutside)
      val points_set_size = bordered_set.filter(t=>t.getPosition(point) == PointPosition.LaysOnNSphere).size
      println("validation step: " + bordered_set.size + " " + points_set_size + " " + point)

      if (bordered_set.size > points_set_size) {
        println("warning! ordered_set.size > points_set_size for  "+point.toString)
        val pp = bordered_set.filter(t => t.getPosition(point) != PointPosition.LaysOnNSphere)
        if (pp.size>0) {
          pp.foreach(
          t => {
            println(t.toString + " :  " + t.getPosition(point) + " " + point +
            " dist: " + t.getPosition_(point) + " , " + t.getDistance(point));
            })
            //return false
            delaunay_false + 1
            }
       else {
         delaunay_false
       }
      }
      else {
        delaunay_false
      }
    }
  }
  println(validation_result)

  validation_result == 0

  //println(tetrahedras.head.innerPoint)
  //true
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
  //simplices1.foreach(x=>writer.write(x.toString+"\n"))
  println("valid: " + validate_data(pointsL.take(n), simplices1.toSeq.distinct))
  writer.close()
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
}

def main(args : Array[String]) : Unit = {
  println(args(0))
  Ut.call_all(args(0).toInt, "output.txt")
}




}
