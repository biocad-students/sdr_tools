import org.scalatest.{Matchers, FlatSpec}

import ru.biocad.ig.common.structures.geometry._

class VectorTests extends FlatSpec with Matchers {
  it should "create Vector instances and do all operations correctly" in {
    val a : Vector = new Vector(Seq(3, 0, 4))
    val b : Vector = new Vector(Seq(4, 0, 3.0))
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
  }
  
  it should "compute unit vector correctly" in {
    val a : Vector = new Vector(Seq(3, 4, 5))
    (math.abs(a.normalize.length - 1.0)) should be < 0.001
  }

  "InfiniteVector" should "be special case" in {
    val InfiniteVector3d = new InfiniteVector(3)
    val InfiniteVector2d = new InfiniteVector(2)
    val b : Vector = new Vector(Seq(4, 0, 3.0))
    Vector2d(0, 0) should not equal (InfiniteVector2d)
    InfiniteVector2d should not equal (InfiniteVector3d)
    InfiniteVector3d should equal (InfiniteVector3d)
    InfiniteVector3d should not equal (Vector3d(0, 0, 0))

    (InfiniteVector3d - b) should equal(InfiniteVector3d)
    (InfiniteVector3d + b) should equal(InfiniteVector3d)
    (b - InfiniteVector3d) should equal(InfiniteVector3d)
    (b + InfiniteVector3d) should equal(InfiniteVector3d)
  }

  it should "support lifting correctly"
}
