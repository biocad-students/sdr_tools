import org.scalatest.{Matchers, FlatSpec}

import spray.json._
import scala.io.Source

import ru.biocad.ig.alascan.constants.json.SidechainLibraryJsonProtocol
import ru.biocad.ig.alascan.constants.json.SidechainLibraryJsonProtocol._

import ru.biocad.ig.alascan.constants.{AminoacidLibrary, SidechainInfo}
import ru.biocad.ig.common.structures.aminoacid.SimplifiedAminoacid
import ru.biocad.ig.common.io.pdb.PDBAtomInfo

class RotamerLibraryTests extends FlatSpec with Matchers {
  it should "restore coordinates with given meshSize" in {
    val rotamerInfo = JsonParser(Source.fromURL(getClass.getResource("/sidechains.json")).getLines().mkString("")).convertTo[AminoacidLibrary[SidechainInfo]]
    //(rotamerInfo.data("LEU")(20)(22)(-32)) should equal (
    //  rotamerInfo.restoreAminoacidInfo("LEU",
    //    20*rotamerInfo.meshSize, 22*rotamerInfo.meshSize, -32*rotamerInfo.meshSize))
  }


  it should "correctly call changeRotamerToRandom for empty SidechainInfo" in {
    val s = SidechainInfo(Seq(), Seq(), 0)
    val aa = SimplifiedAminoacid(Seq(
      PDBAtomInfo(1, "CA", ' ',"ARG", 'L', 2, ' ', 0, 0, 0, 0, 0, "", "C", "")
      ))
    noException should be thrownBy s.changeRotamerToRandom(aa)
    val result = s.changeRotamerToRandom(aa)
    result should equal(aa)
    (result.rotamer) should equal (aa.rotamer)
  }

  "Rotamer database" should "contain data only with non-zero representatives and non-zero vectors" in {
    val rotamerInfo = JsonParser(Source.fromURL(getClass.getResource("/sidechains.json")).getLines().mkString("")).convertTo[AminoacidLibrary[SidechainInfo]]
    rotamerInfo.data.foreach({case (k, v) =>
      v.values.foreach(
        _.values.foreach(
          _.values.foreach({i=>{
            i.representatives should not be empty
            i.representatives.foreach({
              r => //if (k != "ALA" && k != "GLY")
              (r.atoms) should not be empty
            })
            }
          })
        )
      )}
    )
  }
}
