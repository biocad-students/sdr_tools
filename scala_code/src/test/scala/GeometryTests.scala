// import some geometry-related stuff
import org.scalatest.{Matchers, FlatSpec}

import ru.biocad.ig.common.structures.geometry._

import ru.biocad.ig.common.algorithms.geometry.ManifoldUtils

class GeometryTests extends FlatSpec with Matchers {
  it should "create Vector instances and do all operations correctly" in {
    val a : Vector = new Vector(Seq(3, 0, 4))
    val b : Vector = new Vector(Seq(4, 0, 3.0))
    val InfiniteVector3d = new InfiniteVector(3)
    val InfiniteVector2d = new InfiniteVector(2)
    (a - b) should equal (new Vector(Seq(-1, 0, 1)))
    (a + b) should equal (new Vector(Seq(7, 0, 7)))
    (a * b) should be (24)
    (Vector3d(1, 0, 0) ** Vector3d(0, 1, 0)) should equal (Vector3d(0, 0, 1))
    a.length should be(5)
    a.lengthSquared should be(25)
    a.distanceTo(a) should be (0.0)
    a.distanceTo(b) should be (b.distanceTo(a))
    (Vector2d(0, 3)) should equal (new Vector(Seq(0, 3)))
    (Vector3d(0, 1, 2)) should equal (new Vector(Seq(0, 1, 2)))
    (Vector2d(0, 1)) should not equal(Vector3d(0, 1, 2))
    Vector2d(0, 0) should not equal (InfiniteVector2d)
    InfiniteVector2d should not equal (InfiniteVector3d)
    InfiniteVector3d should equal (InfiniteVector3d)
    InfiniteVector3d should not equal (Vector3d(0, 0, 0))

    (InfiniteVector3d - b) should equal(InfiniteVector3d)
    (InfiniteVector3d + b) should equal(InfiniteVector3d)
    (b - InfiniteVector3d) should equal(InfiniteVector3d)
    (b + InfiniteVector3d) should equal(InfiniteVector3d)
  }

  it should "create cofactors for given matrix lines and compute them correctly" in {
    val valuesFunc = ManifoldUtils.getCofactors(
      Seq(
          Seq(1, 2, 3, 4),
          Seq(5, 6, 7, 8),
          Seq(9, 10, 11, 12)
        ))
    valuesFunc(Seq(1, 1, 1, 1)) should be(0.0)
    val valuesFunc2 = ManifoldUtils.getCofactors(
      Seq(
        Seq(0, 2, 3, 4),
        Seq(5, 5, 7, 8),
        Seq(9, 10, 10, 12)
      )
    )
    valuesFunc2(Seq(1, 0, 0, 0)) should be(-12)
    valuesFunc2(Seq(1, 0, 0, 2)) should be(-94)
  }

  it should "have positive, negative or zero determinant values for given test points in 2d" in {
    val a = Vector2d(0, 0)
    val b = Vector2d(20, 0)
    val c = Vector2d(0, 20)
    val d = Vector2d(20, 20)
    val func = ManifoldUtils.getCofactors(Seq(a, b, c).map(_.toSeq))
    func(d.toSeq) should be (0.0)
    func(Vector2d(30, 20).toSeq)*func(InfiniteVector(2).toSeq) should be > 0.0
  }

  it should "have positive, negative or zero determinant values for given test points in 3d"
}
