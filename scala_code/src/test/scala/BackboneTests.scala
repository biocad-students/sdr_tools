import org.scalatest.{Matchers, FlatSpec}

import ru.biocad.ig.alascan.constants.BackboneInfo
import spray.json._
import scala.io.Source

import ru.biocad.ig.alascan.constants.json.BackboneInfoJsonProtocol
import ru.biocad.ig.alascan.constants.json.BackboneInfoJsonProtocol._

class BackboneTests extends FlatSpec with Matchers {
  it should "restore coordinates with given meshSize" in {
    val backboneInfo = JsonParser(Source.fromURL(getClass.getResource("/backbone.json")).getLines().mkString("")).convertTo[BackboneInfo]
    (backboneInfo.data("LEU")(20)(22)(-32)) should equal (
      backboneInfo.restoreCoordinates("LEU",
        20*backboneInfo.meshSize, 22*backboneInfo.meshSize, -32*backboneInfo.meshSize))
  }
}
