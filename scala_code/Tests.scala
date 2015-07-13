package test

import ru.biocad.ig.common.structures.geometry.{
    Vector, Vector2d, Vector3d,InfiniteVector,
    Simplex, Tetrahedra, GeometryVector, Triangle
  }

import ru.biocad.ig.common.algorithms.geometry.{
    ManifoldUtils,
    DelaunayTesselation, DelaunayTesselationFaster, DelaunayTesselation3
  }

import ru.biocad.ig.common.io.pdb.{PDBStructure, PDBAtomInfo}


object StructureTest {
  def main(args : Array[String]) = {
    println("testing")
    val structure : PDBStructure = new PDBStructure()
    structure.readFile("2OSL.pdb")
    println(structure.toString());
  }
}

object VectorTest {
  def main(args : Array[String]) = {
    println("testing")
    val a : Vector = new Vector(Seq(-3, 4))
    val b : Vector = new Vector(Seq(1, 0, 1.0))
    println(a)
    println(b)
    println(a - b)
    println(a + b)
    println(a == b)
    println(a * b)
    println(a.length)
    println(a.lengthSquared)
    println(a.distanceTo(b))
    println(b.distanceTo(a))
    println((a*2).lengthSquared)
    println(a, a*2)
    println(Vector2d(0, 3))
    println(Vector3d(0, 3, 4))

    //println(2*a) -- this doesn't work now
  }
}

object DeterminantTest {
  def main(args : Array[String]) : Unit = {
    println("testing determinant creation")
    val values = ManifoldUtils.getCofactors(
      Seq(
        //Seq(1, 2)
          //Seq(3, 1, 2),
          Seq(1,2,3,4),
          Seq(5,6,7,8),
          Seq(9,10, 11, 12)
        ))
    //List(0,0,0,0)
    val values2 = ManifoldUtils.getCofactors(
      Seq(
        Seq(0,2,3,4),
        Seq(5,5,7,8),
        Seq(9,10, 10, 12)
      )
    )
      println(values2)
      println(values2(Seq(1,0,0,0)))

      println(values2(Seq(1,0,0,2)))
  }
}

object SimplexTest {
  def main(args : Array[String]) : Unit = {
    println("testing simplex properties")
    val a = Vector2d(50, 0)
    val b = Vector2d(30, 40)
    val c = Vector2d(40, 30)
    val s: Simplex = new Simplex(Seq(a, b, c))
    println(s.getPosition(Vector2d(-30, 40)))
    println(s.getPosition(Vector2d(20, 20)))
    println(s.getPosition(Vector2d(50, 50)))
    println(s.getPosition_(Vector2d(50, 50)))
    println("try to iterate")
    s.iterateThroughVertices().foreach(println)
    println("the same in 3d")
    val a1 = Vector3d(0, 0, 0)
    val b1 = Vector3d(0, 0, 100)
    val c1 = Vector3d(100, 0, 0)
    val d1 = Vector3d(0, 100, 0)

    var tetrahedra = new Tetrahedra(a1, b1, c1, d1)
    println(tetrahedra.getPosition(InfiniteVector(3)))
    var tetrahedra2 = new Tetrahedra(a1, b1, d1, c1)
    println(tetrahedra2.getPosition(InfiniteVector(3)))

    val test1 = Seq(Vector3d(21.405, 51.524, -19.856),
      Vector3d(19.759, 55.712, -24.718),
      Vector3d(21.946, 52.459, -20.95),
      Vector3d(18.854, 54.779, -23.918) )
    val tt1 = new Simplex(test1)
    val p1 = Vector3d(23.332, 52.901, -20.613)
    println("3d 2")
    println(tt1.getPosition_(p1))
    println(tt1.getPosition(p1))
    println(tt1.getPosition_(InfiniteVector(3)))
    println(tt1.getPosition(InfiniteVector(3)))
  }
}



object TesselationTest{
  def main(args: Array[String]) = {
    println("testing tesselation")
    val res = new DelaunayTesselation()
/**    res.build(Seq(
      Vector3d(0, 0, 0),
      Vector3d(0, 0, 100),
      Vector3d(100, 0, 0),
      Vector3d(0, 100, 0),
      Vector3d(10, 10, 10)
    ))
  */
  val a = Vector3d(0, 0, 0)
  val b = Vector3d(0, 0, 100)
  val c = Vector3d(100, 0, 0)
  val d = Vector3d(0, 100, 0)

  var tetrahedras = Seq(new Tetrahedra(a, b, c, d))
  println(tetrahedras.head.getPosition(Vector3d(10,10,10)))
  println(tetrahedras.head.getPosition(Vector3d(200,200,200)))

  println(tetrahedras.head.getPosition(a))

  println("testing actual point addition")

  println(
    res.appendPoint(tetrahedras, Vector3d(10, 10, 10))
  )
  println("testing 2 ")
  println(
    res.appendPoint(tetrahedras, Vector3d(100, 100, 100))
  )

  println("testing 3")
  println(
    res.appendPoint(tetrahedras, Vector3d(200, 200, 200))
  )
  println(
    res.prepareBoundingSimplex(Seq(a, b, c, d))
  )

  }
}


object TesselationTest2{
    def main(args: Array[String]) = {
      println("testing tesselation")
      val res = new DelaunayTesselation()
    val a = Vector3d(0, 0, 0)
    val b = Vector3d(0, 0, 100)
    val c = Vector3d(100, 0, 0)
    val d = Vector3d(0, 100, 0)
    val e = Vector3d(10, 10, 10)

    var tetrahedras = Seq(new Tetrahedra(a, b, c, d))

    println("testing actual point addition")
    val points = Seq(a, b, c, d, e)
    res.makeTesselation(points)
    res.simplices.foreach(println)

  }
}

/** basic test for Qhull based triangulation*/
object TesselationTest3{
    def main(args: Array[String]) = {
      println("testing tesselation")
      val res = new DelaunayTesselation3()
    val a = Vector3d(0, 0, 0)
    val b = Vector3d(0, 0, 100)
    val c = Vector3d(100, 0, 0)
    val d = Vector3d(0, 100, 0)
    val e = Vector3d(10, 10, 10)
    val f = Vector3d(100, 100, 100)

    println("testing actual point addition")
    val points = Seq(a, b, c, e, d, f)
    res.makeTesselation(points)
    res.simplices.foreach(println)

  }
}

/** basic test for Qhull based triangulation*/
object TesselationTest3_2d{
    def main(args: Array[String]) = {
      println("testing tesselation")
      val res = new DelaunayTesselation3()
    val a = Vector2d(0, 0)
    val b = Vector2d(0, 100)
    val c = Vector2d(100, 0)
    val d = Vector2d(100, 100)
    val e = Vector2d(10, 10)
    val f = Vector2d(50, 120)

    println("testing actual point addition")
    val points = Seq(a, b, c, e, d, f)
    res.makeTesselation(points)
    res.simplices.foreach(println)

  }
}

object IteratorTest{
  def main(args : Array[String]) = {
    println("testing iteration for simplex parts")
    val a = Vector3d(0, 0, 0)
    val b = Vector3d(0, 0, 100)
    val c = Vector3d(100, 0, 0)
    val d = Vector3d(0, 100, 0)
    val tetrahedras = new Tetrahedra(a, b, c, d)
    println("vertices:")
    tetrahedras.vertices.foreach(println)
    println("pairs of vertices:")
    tetrahedras.getLineSegments().foreach(println)
    println("triangles: ")
    tetrahedras.getTriangles().foreach(println)
    println("ridges: ")
    tetrahedras.getRidges().foreach(println)

    val a2 = Vector2d( 0, 0)
    val b2 = Vector2d(0, 100)
    val c2 = Vector2d(100, 0)
    val d2 = Vector2d(100, 100)
    val triangle = new Simplex(Seq(a2, b2, c2))
    println("vertices:")
    triangle.vertices.foreach(println)
    println("pairs of vertices:")
    triangle.getLineSegments().foreach(println)
    println("triangles: ")
    triangle.getTriangles().foreach(println)
    println("ridges: ")
    triangle.getRidges().foreach(println)
  }
}

object UpdateSimplicesTest{
  def main(args:Array[String]) = {
    val a = Vector3d(0, 0, 0)
    val b = Vector3d(0, 10.0, 10.0)
    val c = Vector3d(10.0, 0, 10.0)
    val d = Vector3d(0, -10.0, 10.0)
    val e1 = Vector3d(0.0, 0.0, 20.0)
    val e2 = Vector3d(0.0, 0.0, 21.0)
    val e3 = Vector3d(0.0, 0.0, 18.0)
    val e4 = Vector3d(0.0, 0.0, 11.0)
    val e5 = Vector3d(5.0, 5.0, 11.0)
    val e6 = Vector3d(2.0, 2.0, 5.0)

    println("tetrahedra:")
    val tetrahedra = new Tetrahedra(a, b, c, d)
    println(tetrahedra)
    println("testing point e1: \n" +  e1.toString)
    println(tetrahedra.getPosition(e1))
    println(ManifoldUtils.updateSimplices(tetrahedra, e1))


    println("testing point e2: \n" +  e2.toString)
    println(tetrahedra.getPosition(e2))
    println(ManifoldUtils.updateSimplices(tetrahedra, e2))


    println("testing point e3: \n" +  e3.toString)
    println(tetrahedra.getPosition(e3))
    println(ManifoldUtils.updateSimplices(tetrahedra, e3))


    println("testing point e4: \n" +  e4.toString)
    println(tetrahedra.getPosition(e4))
    println(ManifoldUtils.updateSimplices(tetrahedra, e4))


    println("testing point e5: \n" +  e5.toString)
    println(tetrahedra.getPosition(e5))
    println(ManifoldUtils.updateSimplices(tetrahedra, e5))


    println("testing point e6: \n" +  e6.toString)
    println(tetrahedra.getPosition(e6))
    println(ManifoldUtils.updateSimplices(tetrahedra, e6))
  }
}

object UpdateSimplicesTest2{
  def main(args:Array[String]) = {
    val a = Vector3d(10.0, 0.0, 0.0)
    val b = Vector3d(0.0, 10.0, 0.0)
    val c = Vector3d(0.0, 0.0, 10.0)
    val d = Vector3d(0.0, -10.0, 0.0)
    val e = Vector3d(0.0, 0.0, -10.0)
    val f = Vector3d(8.0, 6.0, 0.0)
    val x = Vector3d(-10.0, 0.0, 0.0)


    println("tetrahedra:")
    val tetrahedras = Seq(new Tetrahedra(a, b, c, d), new Tetrahedra(a, b ,d, e))
    val t2 =ManifoldUtils.updateSimplices(tetrahedras, f)
    println(t2)
    println(ManifoldUtils.updateSimplices(t2, x))
  }
}

object EqualityTest{
  def main(args:Array[String]) = {
    println("testing equality")
    val a = Vector3d(0, 0, 0)
    val b = Vector3d(0, 0, 100)
    val c = Vector3d(100, 0, 0)
    val d = Vector3d(0, 100, 0)
    val t1 = new Tetrahedra(a, b, c, d)
    val t2 = new Tetrahedra(a, c, b, d)
    println(t1.equals(t2))
    println(t2.equals(t1))
    println("finding distinct tetrahedras")
    println(Set(t1, t2))
    println("finding distinct tetrahedras 2")
    println(Seq(t1, t2).toSet.toSeq)
  }
}

object CompareTetrahedrizationTest{

  def init() : (Seq[GeometryVector], Seq[GeometryVector]) = {
    val structure : PDBStructure = new PDBStructure()
    structure.readFile("empty.pdb")
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

  def main(args : Array[String]) = {
    println("start ")
    val (pointsL, pointsH) = init()
    val n = 9
    val dataset = pointsL.take(n)
    println("preparing 1st tesselation")
    val res1 = new DelaunayTesselation()
    res1.makeTesselation(dataset)
    val s1 = res1.simplices
    val res2 = new DelaunayTesselationFaster()
    res2.makeTesselation(dataset)
    val s2 = res2.simplices
    val res3 = new DelaunayTesselation3()
    res3.makeTesselation(dataset)
    val s3 = res3.simplices
    println("got 3 datasets: " + s1.size + " " + s2.size + " " + s3.size)
    println("dataset1")
    println(s1)
    //println("dataset2")
    //println(s2)
    println("dataset3")
    println(s3)
    println("diff between d2 and d3")
    println(s1.toSet.diff(s2.toSet).toSeq)
    println("1 and 3:")
    println(s1.toSet.diff(s3.toSet).toSeq)
  }
}
