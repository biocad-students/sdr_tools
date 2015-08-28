import org.scalatest.{Matchers, FlatSpec}

import spray.json._
import scala.io.Source

import ru.biocad.ig.alascan.constants.json.RotamerLibraryJsonProtocol
import ru.biocad.ig.alascan.constants.json.RotamerLibraryJsonProtocol._

import ru.biocad.ig.alascan.constants.{AminoacidLibrary, RotamerInfo}

class RotamerLibraryTests extends FlatSpec with Matchers {
  it should "restore coordinates with given meshSize" in {
    val rotamerInfo = JsonParser(Source.fromURL(getClass.getResource("/sidechains.json")).getLines().mkString("")).convertTo[AminoacidLibrary[RotamerInfo]]
    (rotamerInfo.data("LEU")(20)(22)(-32)) should equal (
      rotamerInfo.restoreInfo("LEU",
        20*rotamerInfo.meshSize, 22*rotamerInfo.meshSize, -32*rotamerInfo.meshSize))
  }
}
