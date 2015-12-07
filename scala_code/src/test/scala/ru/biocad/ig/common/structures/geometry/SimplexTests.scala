import org.scalatest.{Matchers, FlatSpec}

import ru.biocad.ig.common.structures.geometry._

import ru.biocad.ig.common.algorithms.geometry.ManifoldUtils

class SimplexTests extends FlatSpec with Matchers {
  it should "not depend on vertices order" in {
    val a = Vector3d(0, 0, 0)
    val b = Vector3d(0, 0, 100)
    val c = Vector3d(100, 0, 0)
    val d = Vector3d(0, 100, 0)
    val t1 = new Tetrahedra(a, b, c, d)
    val t2 = new Tetrahedra(a, c, b, d)
    (t1) should equal (t2)
    (t2) should equal (t1)
    (t1.hasVertex(Vector3d(0.0, 0, 0))) should be (true)
    /**
    println("finding distinct tetrahedras")
    println(Set(t1, t2))
    println("finding distinct tetrahedras 2")
    println(Seq(t1, t2).toSet.toSeq)
    //TODO: rewrite this
    */
  }
}
