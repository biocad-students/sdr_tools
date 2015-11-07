package ru.biocad.ig.common.structures.geometry

import ru.biocad.ig.common.structures.aminoacid._
import ru.biocad.ig.alascan.constants._
import ru.biocad.ig.common.algorithms.geometry.ManifoldUtils
import ru.biocad.ig.common.io.pdb.{PDBAtomInfo}


import ru.biocad.ig.alascan.constants.{AminoacidLibrary, BackboneInfo, SidechainInfo}
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


import ru.biocad.ig.alascan.constants.json.{BasicVectorLibrary,
  BasicVectorLibraryJsonProtocol}
import BasicVectorLibraryJsonProtocol._
import ru.biocad.ig.common.algorithms.geometry.AminoacidUtils
import ru.biocad.ig.common.structures.aminoacid.SimplifiedChain
import ru.biocad.ig.alascan.energies._


object Lattice {
  //
  //val rotamerLibrary = JsonParser(Source.fromURL(getClass.getResource("/sidechains.json")).getLines().mkString("")).convertTo[AminoacidLibrary[SidechainInfo]]

  val backboneVectors : Array[GeometryVector] = JsonParser(
      Source.fromURL(getClass.getResource("/basic_vectors.json")
    ).getLines().mkString("")).convertTo[BasicVectorLibrary].vectors.map(new Vector(_)).toArray


  val backboneInfo = JsonParser(Source.fromURL(getClass.getResource("/backbone.json")).getLines().mkString("")).convertTo[AminoacidLibrary[BackboneInfo]]
  val sidechainsInfo = JsonParser(
    Source.fromURL(
      getClass.getResource("/sidechains.json")).getLines().mkString("")).convertTo[AminoacidLibrary[SidechainInfo]]

  val rotamerRadiusInfo : RotamerRadiusInfo = JsonParser(Source.fromURL(getClass.getResource("/MCDP_json/RADIJC.json")).getLines().mkString("")).convertTo[RotamerRadiusInfo]

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
  /** helper methods*/
  //this returns true if they can, false otherwise - quite simple
  def canFormHBond(aminoacids : SimplifiedChain, i : Int, j : Int) : Boolean = {
    val r_ij = aminoacids(j).ca - aminoacids(i).ca
    if (i == 0 || j == 0)
      return false
    val b_i_b_i_1 = aminoacids(i - 1).ca - aminoacids(i).ca //TODO: check if i == 0
    val b_j_b_j_1 = aminoacids(j - 1).ca - aminoacids(j).ca
    (i - j).abs >= 3 && LatticeConstants.H_bond_distance_condition(r_ij.length) &&
      (b_i_b_i_1*r_ij).abs <= LatticeConstants.H_bond_a_max &&
      (b_j_b_j_1*r_ij).abs <= LatticeConstants.H_bond_a_max
    //LatticeConstants
  }

  /** energy methods*/
  val caTraceEnergyTerm  = new CaTraceEnergy()
  val hBondEnergyTerm = new HydrogenBondEnergy(canFormHBond)
  val rotamerEnergyTerm = new RotamerEnergy()
  val sgLocalEnergyTerm = new SGLocalEnergy()
  val pairEnergyTerm = new PairEnergy(rotamerRadiusInfo)
  val burialEnergyTerm = new BurialEnergy(buildContactMap)
  val templateEnergyTerm = new TemplateEnergy(buildContactMap)

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
  def toFullAtomRepresentation(aminoacids : Seq[SimplifiedAminoacid], originalFullAtomChain : Seq[Seq[PDBAtomInfo]]) : Seq[PDBAtomInfo] = {
    val vectors = (aminoacids.tail, aminoacids).zipped.map(_.ca - _.ca)
    val vectorsWithEdgeOnes = (vectors.head +: vectors) ++ Seq(vectors.init.last, vectors.last)
    val pdbData = (aminoacids, vectorsWithEdgeOnes.sliding(3, 1).toSeq, originalFullAtomChain).zipped.flatMap({
      case (aa, Seq(v1, v2, v3), atoms) => {
        val updatedMap = Map("CA" -> aa.ca * LatticeConstants.MESH_SIZE) ++
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


  def toFullAtomRepresentation(aminoacids : Seq[SimplifiedAminoacid]) = {
    val vectors = (aminoacids.tail, aminoacids).zipped.map(_.ca - _.ca)
    val vectorsWithEdgeOnes = (vectors.head +: vectors) ++ Seq(vectors.init.last, vectors.last)
    val pdbData = (aminoacids, vectorsWithEdgeOnes.sliding(3, 1).toSeq, Stream from 1).zipped.flatMap({
      case (aa, Seq(v1, v2, v3), aaIndex) => {
        val updatedMap = Map("CA" -> aa.ca * LatticeConstants.MESH_SIZE) ++
            restoreInfoCoordinates(aa, v1, v2, v3, backboneInfo) ++
            restoreInfoCoordinates(aa, v1, v2, v3, sidechainsInfo)
        updatedMap.map({
          case (k, v) => {
            (aaIndex, aa.name, k, v)
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
    fragmentInfo.restorePDBInfo(aa, d1, d2, d3, x, y, z, atomsMap)
  }
  def restoreInfoCoordinates[T <: AminoacidFragment](
      aa : SimplifiedAminoacid,
      v1 : GeometryVector,
      v2 : GeometryVector,
      v3 : GeometryVector,
      fragmentInfo : AminoacidLibrary[T]) : Map[String, GeometryVector] = {
    val (d1, d2, d3) = AminoacidUtils.getDistances(v1, v2, v3)
    val (x, y, z) = AminoacidUtils.getLocalCoordinateSystem(v1, v2, v3)
    fragmentInfo.restoreCoordinates(aa, d1, d2, d3, x, y, z)
  }

  def validateStructure(structure : SimplifiedChain) : Boolean = {
    val r0 = (0 to structure.size - 2).forall({
      i => backboneVectors.contains(structure(i + 1).ca - structure(i).ca)
    })
    if (!r0)
      return false
    val r1 = (0 to structure.size - 3).forall({
      i => {
        val a1 = structure(i).ca
        val a2 = structure(i + 1).ca
        val a3 = structure(i + 2).ca
        val v1 = a1 - a2
        val v2 = a3 - a2
        val angle = v1.angleTo(v2)
        angle >= 72.5 && angle <= 154
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
            a1.distanceTo(a2) >= 3.45
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
        a1.distanceTo(a2) >= 4.05
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
    v2.angleTo(v1) >= 72.5 && v2.angleTo(v1) <= 154 && (v1 + v2 + v3).length >= 4.05
  }
}
