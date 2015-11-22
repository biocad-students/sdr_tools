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



class Lattice {
  def loadFromFile[T : JsonReader](fileName : String) : T = {
    val source = Source.fromURL(getClass.getResource(fileName))
    val result = JsonParser(source.getLines().mkString("")).convertTo[T]
    source.close()
    result
  }
  //
  val latticeConstants : LatticeConstants = loadFromFile[LatticeConstants]("/lattice_params.json")

  val backboneVectors : Array[GeometryVector] = loadFromFile[BasicVectorLibrary]("/basic_vectors.json").vectors.map(new Vector(_)).toArray


  val backboneInfo = loadFromFile[AminoacidLibrary[BackboneInfo]]("/backbone.json")
  val sidechainsInfo = loadFromFile[AminoacidLibrary[SidechainInfo]]("/sidechains.json")

  val rotamerRadiusInfo = loadFromFile[RotamerRadiusInfo]("/MCDP_json/RADIJC.json")

  /*
  return value indicates that aminoacids i and j are in contact
  array can be aminoacid-pair specific.
  now there is no KDTree, just simple and slow code - should replace it
  */
  def buildContactMap(chain : SimplifiedChain) : Array[Array[Boolean]] = {
    chain.map({case aa1 => chain.map({
      aa2 => aa2.isInContactWith(aa1, rotamerRadiusInfo.getR(aa1.name, aa2.name))
    }).toArray}).toArray
  }


  /** energy methods*/
  val caTraceEnergyTerm  = new CaTraceEnergy(this)
  val hBondEnergyTerm = new HydrogenBondEnergy(this)
  val rotamerEnergyTerm = new RotamerEnergy(this)
  val sgLocalEnergyTerm = new SGLocalEnergy(this)
  val pairEnergyTerm = new PairEnergy(this)
  val burialEnergyTerm = new BurialEnergy(this)
  val templateEnergyTerm = new TemplateEnergy(this)

  //TODO: we have pair of chains, that means we somehow should utilize that when we compute total energy
  def getEnergy(aminoacids : SimplifiedChain) : Double = {
    0.25 * caTraceEnergyTerm.get(aminoacids) +
     hBondEnergyTerm.get(aminoacids) +
    0.5 * rotamerEnergyTerm.get(aminoacids) +
    1.0 * sgLocalEnergyTerm.get(aminoacids) +
    0.5 * burialEnergyTerm.get(aminoacids)  +
    5 * pairEnergyTerm.get(aminoacids) +
    4.25 * templateEnergyTerm.get(aminoacids)
  }

  //val backboneInfo = JsonParser(Source.fromURL(getClass.getResource("/backbone.json")).getLines().mkString("")).convertTo[AminoacidLibrary[BackboneInfo]]

  /**Returns full-atom representation for given simplified aminoacid
    */
  def toFullAtomRepresentation(chain : SimplifiedChain, originalFullAtomChain : Seq[Seq[PDBAtomInfo]]) : Seq[PDBAtomInfo] = {

    val vectorsWithEdgeOnes = (chain.vectors.head +: chain.vectors) ++ Seq(chain.vectors.init.last, chain.vectors.last)
    val pdbData = (chain.structure, vectorsWithEdgeOnes.sliding(3, 1).toSeq, originalFullAtomChain).zipped.flatMap({
      case (aa, Seq(v1, v2, v3), atoms) => {
        val updatedMap = Map("CA" -> aa.ca * latticeConstants.meshSize) ++
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
        val updatedMap : Map[String, GeometryVector] = Map("CA" -> aa.ca * latticeConstants.meshSize) ++
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
    val r1 = (0 to structure.size - 3).forall({
      i => {
        val a1 = structure(i).ca
        val a2 = structure(i + 1).ca
        val a3 = structure(i + 2).ca
        val v1 = a1 - a2
        val v2 = a3 - a2
        latticeConstants.checkAngleRestrictions(v1.angleTo(v2))
      }
    })
    if (!r1)
        return false
    val r2 = (0 to structure.size - 3).forall({
      i => {
        (i + 2 to structure.size - 1).forall({
          j => {
            val a1 = structure(i).ca
            val a2 = structure(j).ca
            a1.distanceTo(a2) >= latticeConstants.caMinDistance //TODO: check if here should be different value, vas 3.45 instead of 4.05
          }
        })
      }
    })
    if (!r2)
      return false
    val r3 = (0 to structure.size - 4).forall({
      i => {
        val a1 = structure(i).ca
        val a2 = structure(i + 3).ca
        a1.distanceTo(a2) >= latticeConstants.caMinDistance
      }
    })
    r3
  }

  def prepareValidVectors(n : Int) : Seq[GeometryVector] = {
    //1. should get random pair of vector such as structure validation conditions holds.
    val l1 = Vector3d(3, 1, 0)
    val l2 = Vector3d(3, -1, 0)
    //2. then repeat them continually till the coil is received
    Stream.continually(Seq(l1, l2).toStream).flatten.take(n).toSeq
  }

  def validateVectors(v1 : GeometryVector, v2 : GeometryVector, v3 : GeometryVector) = {
    latticeConstants.checkAngleRestrictions(v2.angleTo(v1)) && (v1 + v2 + v3).length >= latticeConstants.caMinDistance
  }
}
