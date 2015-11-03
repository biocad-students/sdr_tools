import org.scalatest.{Matchers, FlatSpec}

import spray.json._
import scala.io.Source

import ru.biocad.ig.alascan.constants.json.BackboneLibraryJsonProtocol
import ru.biocad.ig.alascan.constants.json.BackboneLibraryJsonProtocol._

import ru.biocad.ig.alascan.constants.{AminoacidLibrary, BackboneInfo}

class BackboneTests extends FlatSpec with Matchers {
  it should "restore coordinates with given meshSize" in {
    val backboneInfo = JsonParser(Source.fromURL(getClass.getResource("/backbone.json")).getLines().mkString("")).convertTo[AminoacidLibrary[BackboneInfo]]
    //(backboneInfo.data("LEU")(20)(22)(-32)) should equal (
    //  backboneInfo.restoreAminoacidInfo("LEU",
    //    20*backboneInfo.meshSize, 22*backboneInfo.meshSize, -32*backboneInfo.meshSize))
  }

  "Backbone database" should "contain only non-zero data" in {
    val backboneInfo = JsonParser(Source.fromURL(getClass.getResource("/backbone.json")).getLines().mkString("")).convertTo[AminoacidLibrary[BackboneInfo]]
    backboneInfo.data.values.foreach(
      _.values.foreach(
        _.values.foreach(
          _.values.foreach(_.data should not be empty)
        )
      )
    )
  }

}
