package ru.biocad.ig.common.structures.geometry

import ru.biocad.ig.common.structures.aminoacid._
import ru.biocad.ig.alascan.constants._
import ru.biocad.ig.common.algorithms.geometry.ManifoldUtils
import ru.biocad.ig.common.io.pdb.{PDBAtomInfo}


import ru.biocad.ig.alascan.constants._
import spray.json._
//import ru.biocad.ig.alascan.constants.json.BackboneLibraryJsonProtocol
import ru.biocad.ig.alascan.constants.json.BackboneLibraryJsonProtocol._
//import ru.biocad.ig.alascan.constants.json.SidechainLibraryJsonProtocol
import ru.biocad.ig.alascan.constants.json.SidechainLibraryJsonProtocol._
//import DefaultJsonProtocol._
import scala.io.Source

import ru.biocad.ig.alascan.constants.energy_terms._
import EOneJsonProtocol._

import ERotamerJsonProtocol._
import RotamerRadiusInfoJsonProtocol._
import LatticeConstantsJsonProtocol._

import ru.biocad.ig.alascan.constants.json.{BasicVectorLibrary,
  BasicVectorLibraryJsonProtocol}
import BasicVectorLibraryJsonProtocol._
import ru.biocad.ig.common.algorithms.geometry.AminoacidUtils
import ru.biocad.ig.common.structures.aminoacid.SimplifiedChain
import ru.biocad.ig.alascan.energies._

import java.io._
import java.util.zip.GZIPInputStream

case class Lattice(val settingsFile : File) {
  def this(fileName: String) = this(new File(fileName))

  val settingsDirectory : String = settingsFile.getAbsoluteFile().getParent() match {
    case f : String => f
    case null => ""
  }
  println(settingsDirectory)

  def loadFromFile[T : JsonReader](fileName : String) : T = {
    val filePath = Seq(settingsDirectory, fileName).mkString(File.separatorChar.toString)
    fileName.split('.').last match {
      case "gz" => loadFromFileGZ[T](filePath)
      case _ => loadFromFile[T](new File(filePath))
    }
  }

  def loadFromFileGZ[T : JsonReader](filePath : String) : T = {
    val source = Source.fromInputStream(new GZIPInputStream(new BufferedInputStream(new FileInputStream(filePath))))
    val result = JsonParser(source.getLines().mkString("")).convertTo[T]
    source.close() //todo: find out if i should close GZIPInputStream and all other stuff
    result
  }

  def loadFromFile[T : JsonReader](file : File) : T = {
    val source = Source.fromFile(file)
    val result = JsonParser(source.getLines().mkString("")).convertTo[T]
    source.close()
    result
  }


  //
  val latticeConstants : LatticeConstants = loadFromFile[LatticeConstants](settingsFile)

  val backboneVectors : Array[GeometryVector] = loadFromFile[BasicVectorLibrary](
    latticeConstants.parameters("basic_vectors")).vectors.map(new Vector(_)).toArray


  val backboneInfo = loadFromFile[AminoacidLibrary[BackboneInfo]](
    latticeConstants.parameters("backbone_library"))

  val sidechainsInfo = loadFromFile[AminoacidLibrary[SidechainInfo]](
    latticeConstants.parameters("sidechain_library"))

  val rotamerRadiusInfo = loadFromFile[RotamerRadiusInfo](
    latticeConstants.parameters("rotamer_info"))


  //TODO: we have pair of chains, that means we somehow should utilize that when we compute total energy
  val getEnergy = latticeConstants.buildEnergyFunction(this)
  //val backboneInfo = JsonParser(Source.fromURL(getClass.getResource("/backbone.json")).getLines().mkString("")).convertTo[AminoacidLibrary[BackboneInfo]]

  /**Returns full-atom representation for given simplified aminoacid
    */
  def toFullAtomRepresentation(chain : SimplifiedChain, originalFullAtomChain : Seq[Seq[PDBAtomInfo]]) : Seq[PDBAtomInfo] = {

    val vectorsWithEdgeOnes = (chain.vectors.head +: chain.vectors) ++ Seq(chain.vectors.init.last, chain.vectors.last)
    val pdbData = (chain.structure, vectorsWithEdgeOnes.sliding(3, 1).toSeq, originalFullAtomChain).zipped.flatMap({
      case (aa, Seq(v1, v2, v3), atoms) => {
        val updatedMap = Map("CA" -> aa.caInLatticeCoordinates * latticeConstants.meshSize) ++
            restoreInfoCoordinates(aa, v1, v2, v3, backboneInfo) ++
            restoreInfoCoordinates(aa, v1, v2, v3, sidechainsInfo)
        atoms.map({atom =>
          if (updatedMap.contains(atom.atom))
              SimplifiedAminoacid.getUpdatedAtomInfo(updatedMap(atom.atom), atom)
          else atom
        })
      }
    })
    //pdbData.foreach(println)
    println("done all")
    pdbData
  }


  def toFullAtomRepresentation(chain : SimplifiedChain) = {
    val vectorsWithEdgeOnes = (chain.vectors.head +: chain.vectors) ++ Seq(chain.vectors.init.last, chain.vectors.last)
    val backboneAtomsOrder = Seq("N", "CA", "C", "O")
    val pdbData = (chain.structure, vectorsWithEdgeOnes.sliding(3, 1).toSeq, Stream from 1).zipped.flatMap({
      case (aa, Seq(v1, v2, v3), aaIndex) => {
        val updatedMap : Map[String, GeometryVector] = Map("CA" -> aa.caInLatticeCoordinates * latticeConstants.meshSize) ++
            restoreInfoCoordinates(aa, v1, v2, v3, backboneInfo) ++
            restoreInfoCoordinates(aa, v1, v2, v3, sidechainsInfo)

        (backboneAtomsOrder ++ updatedMap.keys.filterNot(backboneAtomsOrder.contains(_)).toSeq.sorted).map({
          case k => {
            (aaIndex, aa.name, k, updatedMap(k))
          }
        })
      }
    })
    val result = (pdbData, Stream from 1).zipped.map({
      case ((aaIndex, aaName, k, v), index) => PDBAtomInfo(index, k, aaName, 'A', aaIndex, v)
    })
    println("done all")
    result
  }

  def restoreInfoFragment[T <: AminoacidFragment](
      aa : SimplifiedAminoacid,
      v1 : GeometryVector,
      v2 : GeometryVector,
      v3 : GeometryVector,
      fragmentInfo : AminoacidLibrary[T],
      atomsMap : Map[String, PDBAtomInfo]) : Seq[PDBAtomInfo] = {
    val (d1, d2, d3) = AminoacidUtils.getDistances(v1, v2, v3)
    val (x, y, z) = AminoacidUtils.getLocalCoordinateSystem(v1, v2, v3)
    fragmentInfo.restorePDBInfo(aa, d1, d2, d3, x, y, z, atomsMap, this)
  }
  def restoreInfoCoordinates[T <: AminoacidFragment](
      aa : SimplifiedAminoacid,
      v1 : GeometryVector,
      v2 : GeometryVector,
      v3 : GeometryVector,
      fragmentInfo : AminoacidLibrary[T]) : Map[String, GeometryVector] = {
    val (d1, d2, d3) = AminoacidUtils.getDistances(v1, v2, v3)
    val (x, y, z) = AminoacidUtils.getLocalCoordinateSystem(v1, v2, v3)
    fragmentInfo.restoreCoordinates(aa, d1, d2, d3, x, y, z, this)
  }

  def validateStructure(structure : SimplifiedChain) : Boolean = {
    val r0 = structure.vectors.forall({v => backboneVectors.contains(v) })
    if (!r0)
      return false
    val r1 = structure.vectors.sliding(2, 1).forall({
      case Seq(v1, v2) => {
        latticeConstants.checkAngleRestrictions(v1.angleTo(-v2))
      }
    })
    if (!r1)
        return false
    val r2 = (0 to structure.size - 3).forall({
      i => {
        (i + 2 to structure.size - 1).forall({
          j => {
            val a1 = structure(i).caInLatticeCoordinates
            val a2 = structure(j).caInLatticeCoordinates
            a1.distanceTo(a2) >= latticeConstants.caMinDistance //TODO: check if here should be different value, vas 3.45 instead of 4.05
          }
        })
      }
    })
    r2
  }

  //TODO: rewrite this method
  def prepareValidVectors(n : Int) : Seq[GeometryVector] = {
    //1. should get random pair of vector such as structure validation conditions holds.
    val l1 = Vector3d(3, 1, 0)
    val l2 = Vector3d(3, -1, 0)
    //2. then repeat them continually till the coil is received
    Stream.continually(Seq(l1, l2).toStream).flatten.take(n).toSeq
  }

  def validateVectors(v1 : GeometryVector, v2 : GeometryVector, v3 : GeometryVector) = {
    latticeConstants.checkAngleRestrictions(v2.angleTo(-v1)) && (v1 + v2 + v3).length >= latticeConstants.caMinDistance
  }

}
