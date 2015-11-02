import org.scalatest.{Matchers, FlatSpec}

import spray.json._
import scala.io.Source

import ru.biocad.ig.alascan.constants.json.SidechainLibraryJsonProtocol
import ru.biocad.ig.alascan.constants.json.SidechainLibraryJsonProtocol._

import ru.biocad.ig.alascan.constants.{AminoacidLibrary, SidechainInfo}
import ru.biocad.ig.common.structures.aminoacid.SimplifiedChain


class StructureGenerationTests extends FlatSpec with Matchers {
  it should "generate simplified structure from sequence" in {
    val rotamerInfo = JsonParser(Source.fromURL(getClass.getResource("/sidechains.json")).getLines().mkString("")).convertTo[AminoacidLibrary[SidechainInfo]]
    noException should be thrownBy{
      SimplifiedChain.fromSequence("GBARFIELD", rotamerInfo)
    }
  }
}
