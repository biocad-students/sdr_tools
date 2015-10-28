import org.scalatest.{Matchers, FlatSpec}

import spray.json._
import scala.io.Source

import ru.biocad.ig.common.io.pdb.PDBAtomInfo

class PDBAtomInfoTests extends FlatSpec with Matchers {
  it should "save and restore PDBAtomInfo from/to String correctly" in {
    val exampleInfo = PDBAtomInfo(1, "CA", '+',"ARG", 'L', 2, 'f', 0, 0, 0, 0,0, "ABDE", "C", "TT")
    //added mess values to rarely used fields to ensure they are (de)serialized properly
    (exampleInfo.toString.length) should equal (80)

    noException should be thrownBy {
      PDBAtomInfo(exampleInfo.toString)
    }
    val deserializedInfo = PDBAtomInfo(exampleInfo.toString)
    (exampleInfo.productIterator.toSeq, deserializedInfo.productIterator.toSeq).zipped.foreach((_) should equal (_))
  }
}
