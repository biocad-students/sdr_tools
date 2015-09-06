package ru.biocad.ig.common.structures.geometry

import ru.biocad.ig.common.structures.aminoacid._
import ru.biocad.ig.alascan.constants._
import ru.biocad.ig.common.algorithms.geometry.ManifoldUtils

import spray.json._
//import DefaultJsonProtocol._
import scala.io.Source

import ru.biocad.ig.alascan.constants.energy_terms._
import EOneJsonProtocol._
import E14JsonProtocol._
import E14avgJsonProtocol._

object Lattice {
  val eone : EOne = JsonParser(Source.fromURL(getClass.getResource("/MCDP_json/EONE.json")).getLines().mkString("")).convertTo[EOne]
  val e14 : E14 = JsonParser(Source.fromURL(getClass.getResource("/MCDP_json/r14aa12.json")).getLines().mkString("")).convertTo[E14]
  val e14avg : E14avg = JsonParser(Source.fromURL(getClass.getResource("/MCDP_json/r14avg12.json")).getLines().mkString("")).convertTo[E14avg]

  /*
  return value indicates that aminoacids i and j are in contact
  array can be aminoacid-pair specific.
  now there is no KDTree, just simple and slow code - should replace it
  */
  def buildContactMap(aminoacids : Seq[SimplifiedAminoAcid]) : Array[Array[Boolean]] = {
    aminoacids.map({case aa1 => aminoacids.map(_.isInContactWith(aa1)).toArray}).toArray
  }

  /** helper methods*/
  //this returns true if they can, false otherwise - quite simple
  def can_form_H_bond(aminoacids : Seq[SimplifiedAminoAcid], i : Int, j : Int) : Boolean = {
    val r_ij = aminoacids(j).ca - aminoacids(i).ca
    val b_i_b_i_1 = aminoacids(i - 1).ca - aminoacids(i).ca //TODO: check if i == 0
    val b_j_b_j_1 = aminoacids(j - 1).ca - aminoacids(j).ca
    i - j >= 3 && LatticeConstants.H_bond_distance_condition(r_ij.length) &&
      (b_i_b_i_1*r_ij).abs <= LatticeConstants.H_bond_a_max &&
      (b_j_b_j_1*r_ij).abs <= LatticeConstants.H_bond_a_max
    //LatticeConstants
  }

  /** energy methods*/
  def get_E_CA_trace(aminoacids : Array[SimplifiedAminoAcid]) : Double = {
    val r14Seq : Seq[Double] = (1 to aminoacids.size - 3).map({
      case i : Int =>
      {
        val b = (Seq(i, i + 1, i + 2), Seq(i - 1, i, i + 1)).zipped.map({
          case (x : Int, y : Int) => {
            aminoacids(x).ca - aminoacids(y).ca
          }
        })
        b.reduceLeft(_ + _).lengthSquared * math.signum(ManifoldUtils.getDeterminant(b.map(_.coordinates)))
      }})
    (r14Seq, 0.0 +: r14Seq, (1 to aminoacids.size - 3)).zipped.map({
      case (r14, r14_prev, i)  => {
        3*e14.get(r14, aminoacids(i).name, aminoacids(i + 1).name) +
          e14avg.get(r14, r14_prev)
      }
    }).reduceLeft(_ + _)
  }

  def get_E_H_bond(aminoacids : Seq[SimplifiedAminoAcid]) : Double = {
    var E = 0.0
    (1 to aminoacids.size).foreach( i => {
      (i to aminoacids.size).foreach(j => {
        if (can_form_H_bond(aminoacids, i, j)) {
          E += LatticeConstants.E_H
          //FIX: consider cooperativity
          //FIX: consider condition - each aminoacid can form no more than 2 H-bonds (proline - no more than 1 bond)
          //FIX: maximize
        }
      })
    }
    )
    E
  }

  def get_E_rot(aminoacids : Seq[SimplifiedAminoAcid]): Double = {
    ???
  }

  def get_E_SG_local(aminoacids : Seq[SimplifiedAminoAcid]) : Double = {
    ???
  }

  //this is very-very SLOW implementation, should refactor
  def get_E_one(aminoacids : Seq[SimplifiedAminoAcid]) : Double = {
    val contactMap = buildContactMap(aminoacids)
    (0 to aminoacids.size - 1).map({
      i => {
        val numberOfContacts = (i + 1 to aminoacids.size - 1).count({ case j => contactMap(i)(j) })
        eone.get(aminoacids(i).name, numberOfContacts)
      }
    }).reduceLeft(_ + _)
  }

  def get_E_two(i : Int, j : Int, ai : SimplifiedAminoAcid, aj : SimplifiedAminoAcid) : Double = {
    (ai.rotamer.center - aj.rotamer.center).length match {
      //case x if x < rRep(ai.name, aj.name) => eRep
      //case x if x < r(ai.name, aj.name) && epsilon(ai.name)(aj.name) >= 0.0 => epsilon(ai.name)(aj.name)*a(i)(j)
      case _ => 1*//epsilon(ai.name)(aj.name) *
        (if (j - i == 5 || j-i == 6) 0.6 else 1.0)//this is a(i, j)
    }
    ???
  }
  def get_E_pair(aminoacids : Seq[SimplifiedAminoAcid]) : Double = {
    (0 to aminoacids.size - 1).flatMap({
      i => (i + 4 to aminoacids.size - 1).map({
        j => get_E_two(i, j, aminoacids(i), aminoacids(j))
      })
    }).reduceLeft(_ + _)
  }

  def get_E_tem(aminoacids : Seq[SimplifiedAminoAcid]) : Double = {
    ???
  }

  //TODO: we have pair of chains, that means we somehow should utilize that when we compute total energy
  def getEnergy(aminoacids : Array[SimplifiedAminoAcid]) : Double = {
    0.25 * get_E_CA_trace(aminoacids) +
    // get_E_H_bond(aminoacids) +
    //get_E_rot(aminoacids) +
    1.0 * get_E_SG_local(aminoacids) +
    0.5 * get_E_one(aminoacids)  //+
    //5*get_E_pair(aminoacids)
    //+4.25*get_E_tem(aminoacids)
  }

}
