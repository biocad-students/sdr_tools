package ru.biocad.ig.common.structures.geometry

import ru.biocad.ig.common.structures.aminoacid._
import ru.biocad.ig.alascan.constants._
import ru.biocad.ig.common.algorithms.geometry.ManifoldUtils
import ru.biocad.ig.common.algorithms.geometry.HydrogenBondsFinder
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
import EPairJsonProtocol._
import E14JsonProtocol._
import E14avgJsonProtocol._
import ESgLocalJsonProtocol._
import ERotamerJsonProtocol._
import RotamerRadiusInfoJsonProtocol._


import ru.biocad.ig.alascan.constants.json.{BasicVectorLibrary, BasicVectorLibraryJsonProtocol}
import BasicVectorLibraryJsonProtocol._
import ru.biocad.ig.common.algorithms.geometry.AminoacidUtils
import ru.biocad.ig.common.structures.aminoacid.SimplifiedChain

object Lattice {
  val eone : EOne = JsonParser(Source.fromURL(getClass.getResource("/MCDP_json/EONE.json")).getLines().mkString("")).convertTo[EOne]
  val e14 : E14 = JsonParser(Source.fromURL(getClass.getResource("/MCDP_json/r14aa12.json")).getLines().mkString("")).convertTo[E14]
  val e14avg : E14avg = JsonParser(Source.fromURL(getClass.getResource("/MCDP_json/r14avg12.json")).getLines().mkString("")).convertTo[E14avg]
  val eSglocal : ESgLocal = JsonParser(Source.fromURL(getClass.getResource("/MCDP_json/BGB2345.json")).getLines().mkString("")).convertTo[ESgLocal]
  //
  val epair : EPair = JsonParser(Source.fromURL(getClass.getResource("/MCDP_json/PMFHIX_SCALE.json")).getLines().mkString("")).convertTo[EPair]
  val eRotamer : ERotamer = JsonParser(Source.fromURL(getClass.getResource("/MCDP_json/rotamer_energies.json")).getLines().mkString("")).convertTo[ERotamer]
  val rotamerRadiusInfo : RotamerRadiusInfo = JsonParser(Source.fromURL(getClass.getResource("/MCDP_json/RADIJC.json")).getLines().mkString("")).convertTo[RotamerRadiusInfo]
  //val rotamerLibrary = JsonParser(Source.fromURL(getClass.getResource("/sidechains.json")).getLines().mkString("")).convertTo[AminoacidLibrary[SidechainInfo]]

  val backboneVectors : Array[GeometryVector] = JsonParser(
      Source.fromURL(getClass.getResource("/basic_vectors.json")
    ).getLines().mkString("")).convertTo[BasicVectorLibrary].vectors.map(new Vector(_)).toArray


  val backboneInfo = JsonParser(Source.fromURL(getClass.getResource("/backbone.json")).getLines().mkString("")).convertTo[AminoacidLibrary[BackboneInfo]]
  val sidechainsInfo = JsonParser(
    Source.fromURL(
      getClass.getResource("/sidechains.json")).getLines().mkString("")).convertTo[AminoacidLibrary[SidechainInfo]]


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
  def can_form_H_bond(aminoacids : SimplifiedChain, i : Int, j : Int) : Boolean = {
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
  def get_E_CA_trace(aminoacids : SimplifiedChain) : Double = {
    val r14Seq : Seq[Double] = (1 to aminoacids.size - 3).map({
      case i : Int =>
      {
        val b = (Seq(i, i + 1, i + 2), Seq(i - 1, i, i + 1)).zipped.map({
          case (x : Int, y : Int) => aminoacids(x).ca - aminoacids(y).ca
        })
        b.reduceLeft(_ + _).lengthSquared * math.signum(ManifoldUtils.getDeterminant(b.map(_.coordinates)))
      }})
    (r14Seq, 0.0 +: r14Seq, (1 to aminoacids.size - 3)).zipped.map({
      case (r14, r14_prev, i)  => 3*e14.get(r14, aminoacids(i).name, aminoacids(i + 1).name) + e14avg.get(r14, r14_prev)
    }).sum
  }

  def get_E_H_bond(aminoacids : SimplifiedChain) : Double = {
    var E = 0.0
    val b = new HydrogenBondsFinder(can_form_H_bond, aminoacids)
    LatticeConstants.E_HH*b.cooperativeCount + LatticeConstants.E_H*b.bondsCount//TODO: fix this, add cooperativity
  }

  def get_E_rot(aminoacids : SimplifiedChain): Double = {
    (2 to aminoacids.size - 2).map({
      case i => eRotamer.get(aminoacids(i), aminoacids(i - 1), aminoacids(i + 1))
    }).sum
  }

  def get_E_SG_local(aminoacids : SimplifiedChain) : Double = {
    (1 to aminoacids.size - 2).flatMap({
      i => (1 to 4).map({ k => if (i + k < aminoacids.size)
        eSglocal.get(aminoacids(i), aminoacids(i + k), k) else 0.0
        })
    }).sum
  }

  //this is very-very SLOW implementation, should refactor
  def get_E_one(chain : SimplifiedChain) : Double = {
    val contactMap = buildContactMap(chain)
    (0 to chain.size - 1).map({
      i => {
        val numberOfContacts = (i + 1 to chain.size - 1).count({ case j => contactMap(i)(j) })
        eone.get(chain(i).name, numberOfContacts)
      }
    }).sum
  }

  //TODO: rewrite later
  def get_E_two(i : Int, j : Int, ai : SimplifiedAminoacid, aj : SimplifiedAminoacid, f : Double) : Double = {
    val rRepulsive = rotamerRadiusInfo.getRrep(ai.name, aj.name)
    val rInteraction = rotamerRadiusInfo.getR(ai.name, aj.name)
    val pairEnergy = epair.get(ai.name, aj.name)
    (ai.rotamer - aj.rotamer).length match {
      case x if x < rRepulsive => rotamerRadiusInfo.eRepulsive
      case x if x < rInteraction && pairEnergy >= 0.0 && (j - i == 5 || j - i == 6) => pairEnergy * 0.6
      case x if x < rInteraction && pairEnergy >= 0.0 => pairEnergy
      case _ if j - i == 5 || j - i == 6 => pairEnergy * f * 0.6
      case _ => pairEnergy * f
    }
  }

  def get_E_pair(aminoacids : SimplifiedChain) : Double = {
    val constAngle20 = math.pow(math.cos(math.toRadians(20.0)), 2)
    (2 to aminoacids.size - 3).flatMap({
      i => (i + 4 to aminoacids.size - 3).map({
        j => {
          //TODO: check actual +- 2 for f
          val ui_uj = (aminoacids(i + 2).ca - aminoacids(i - 2).ca).normalize *
                      (aminoacids(j + 2).ca - aminoacids(j - 2).ca).normalize
          val f = 1.0 - math.pow(ui_uj * ui_uj - constAngle20, 2)
          get_E_two(i, j, aminoacids(i), aminoacids(j), f)
        }
      })
    }).sum
  }

  def get_E_tem(chain : SimplifiedChain) : Double = {
    val contactMap = buildContactMap(chain)
    //TODO: check borders
    (4 to chain.size - 5).flatMap({
      i => (i + 4 to chain.size - 5).map({
        j => {
          Seq(-4,-3,3,4).map({k => {
            (if (contactMap(i)(j) && contactMap(i + k)(j + k))
              epair.apab(chain(i).name)(chain(j).name) +
              epair.apab(chain(i + k).name)(chain(j + k).name)
            else 0.0)+
            (if (contactMap(i)(j) && contactMap(i + k)(j - k))
              epair.apab(chain(i).name)(chain(j).name) +
              epair.apab(chain(i + k).name)(chain(j - k).name)
            else 0.0)}}).sum
        }
      })
    }).sum
    //???
  }

  //TODO: we have pair of chains, that means we somehow should utilize that when we compute total energy
  def getEnergy(aminoacids : SimplifiedChain) : Double = {
    0.25 * get_E_CA_trace(aminoacids) +
    // get_E_H_bond(aminoacids) +
    0.5 * get_E_rot(aminoacids) +
    1.0 * get_E_SG_local(aminoacids) +
    0.5 * get_E_one(aminoacids)  +
    5 * get_E_pair(aminoacids) +
    4.25 * get_E_tem(aminoacids)
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


  def toFullAtomRepresentation(aminoacids : Seq[SimplifiedAminoacid])  = {
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
    val r2 = (0 to structure.size - 4).forall({
      i => {
        val a1 = structure(i).ca
        val a2 = structure(i + 3).ca
        a1.distanceTo(a2) >= 4.05
      }
    })
    r2
  }

  def prepareValidVectors(n : Int) : Seq[GeometryVector] = {
    //1. should get random pair of vector such as structure validation conditions holds.
    val l1 = Vector3d(2, 1, 0)
    val l2 = Vector3d(2, -1, 0)
    //2. then repeat them continually till the coil is received
    Stream.continually(Seq(l1, l2).toStream).flatten.take(n).toSeq
  }

  def validateVectors(v1 : GeometryVector, v2 : GeometryVector, v3 : GeometryVector) = {
    v2.angleTo(v1) >= 72.5 && v2.angleTo(v1) <= 154 && (v1 + v2 + v3).length >= 4.05
  }
}
