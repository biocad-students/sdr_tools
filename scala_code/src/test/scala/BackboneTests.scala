import org.scalatest.{Matchers, FlatSpec}

import spray.json._
import scala.io.Source

import ru.biocad.ig.common.structures.geometry.Lattice

import ru.biocad.ig.alascan.constants.{AminoacidLibrary, BackboneInfo}

class BackboneTests extends FlatSpec with Matchers {
  it should "restore coordinates with given meshSize" in {
    val lattice = new Lattice("config/lattice_params.json")
    val backboneInfo = lattice.backboneInfo
    //(backboneInfo.data("LEU")(20)(22)(-32)) should equal (
    //  backboneInfo.restoreAminoacidInfo("LEU",
    //    20*backboneInfo.meshSize, 22*backboneInfo.meshSize, -32*backboneInfo.meshSize))
  }

  "Backbone database" should "contain only non-zero data" in {
    val lattice = new Lattice("config/lattice_params.json")
    val backboneInfo = lattice.backboneInfo
    backboneInfo.data.values.foreach(
      _.values.foreach(
        _.values.foreach(
          _.values.foreach(_.data should not be empty)
        )
      )
    )
  }

}
