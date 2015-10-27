// import some geometry-related stuff
import org.scalatest.{Matchers, FlatSpec}

import ru.biocad.ig.common.structures.aminoacid._

import ru.biocad.ig.alascan.constants.json.SidechainLibraryJsonProtocol
import ru.biocad.ig.alascan.constants.json.SidechainLibraryJsonProtocol._

import ru.biocad.ig.alascan.constants.{AminoacidLibrary, SidechainInfo, BackboneInfo}
import ru.biocad.ig.common.structures.aminoacid.SimplifiedAminoacid
import ru.biocad.ig.common.io.pdb.PDBAtomInfo
import ru.biocad.ig.common.algorithms.MonteCarloRunner
import java.util.NoSuchElementException
import ru.biocad.ig.alascan.constants.LatticeConstants
import ru.biocad.ig.common.structures.geometry.{GeometryVector, Vector3d}


class AminoacidsTests extends FlatSpec with Matchers {
  it should "construct simplified aminoacid representation from full-atom model correctly" in {
    //val (simplifiedChain, fullAtomChain) = MonteCarloRunner.loadStructure("/2OSL.pdb")
    a [NoSuchElementException] should be thrownBy {
      SimplifiedAminoacid(Seq(
        PDBAtomInfo(1, "CB", ' ',"ARG", 'L', 2, ' ', 0, 0, 0, 0,0, "C", "")
        ))
    }
    noException should be thrownBy {
      SimplifiedAminoacid(Seq(
        PDBAtomInfo(1, "CA", ' ',"ARG", 'L', 2, ' ', 0, 0, 0, 0,0, "C", "")
        ))
    }
    //TODO: check input data
  }

  it should "restore backbone atoms from alpha-carbons" in {
    val info = PDBAtomInfo(1, "CA", ' ',"ARG", 'L', 2, ' ',
        2*LatticeConstants.MESH_SIZE,
        -10*LatticeConstants.MESH_SIZE,
        0*LatticeConstants.MESH_SIZE, 0,0, "C", "")
    val info2 = PDBAtomInfo(2, "C", ' ',"ARG", 'L', 2, ' ',
            2*LatticeConstants.MESH_SIZE + 0.5,
            -10*2*LatticeConstants.MESH_SIZE+0.5,
            0.5, 0,0, "C", "")
    val aa = SimplifiedAminoacid(Seq(
      info,
      info2
      ))
    val updatedAA = aa.getUpdatedAtomInfo("CA", aa.ca * LatticeConstants.MESH_SIZE, Map(
      "CA" -> info
    ))
    (updatedAA.x) should equal (aa.ca.coordinates(0)*LatticeConstants.MESH_SIZE)
    (updatedAA.y) should equal (aa.ca.coordinates(1)*LatticeConstants.MESH_SIZE)
    (updatedAA.z) should equal (aa.ca.coordinates(2)*LatticeConstants.MESH_SIZE)

    (updatedAA.x) should equal (info.x +- 0.1)
    (updatedAA.y) should equal (info.y +- 0.1)
    (updatedAA.z) should equal (info.z +- 0.1)

    val fragmentInfo = AminoacidLibrary[BackboneInfo](
      Map(
        "ARG" -> Map(1 -> Map(2 -> Map(3 -> BackboneInfo(
          Map("C" -> Vector3d(0.5, 0.5, 0.5))
        ))))
      ),
      LatticeConstants.MESH_SIZE
    )
    val (x, y, z) = (Vector3d(1, 0, 0), Vector3d(0, 1, 0), Vector3d(0, 0, 1))
    val cBackboneInfo : PDBAtomInfo = fragmentInfo.restorePDBInfo(aa, 1, 2, 3, x, y, z, Map(
      "CA" -> info, "C" -> info2
    )).map(x=>x.atom -> x).toMap.getOrElse("C", info)
    (cBackboneInfo.x) should equal (info2.x +- 0.1)
    (cBackboneInfo.y) should equal (info2.y +- 0.1)
    (cBackboneInfo.z) should equal (info2.z +- 0.1)
    (cBackboneInfo.resSeq) should equal (info2.resSeq)
    (cBackboneInfo.serial) should equal (info2.serial)

  }
  it should "restore sidechains from backbone+united atom"
}
