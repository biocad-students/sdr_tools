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

object Lattice {
  val eone : EOne = JsonParser(Source.fromURL(getClass.getResource("/MCDP_json/EONE.json")).getLines().mkString("")).convertTo[EOne]
  val e14 : E14 = JsonParser(Source.fromURL(getClass.getResource("/MCDP_json/r14aa12.json")).getLines().mkString("")).convertTo[E14]

  /** helper methods*/
  //this returns true if they can, false otherwise - quite simple
  def can_form_H_bond(aminoacids: Seq[SimplifiedAminoAcid], i : Int, j : Int) : Boolean = {
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
    (1 to aminoacids.size - 2).map( i =>
      {
        val b = (Seq(i, i + 1, i + 2), Seq(i - 1, i, i + 1)).zipped.map({
          case (x : Int, y : Int) => {
            aminoacids(x).ca - aminoacids(y).ca
          }
        })
        val r14 = math.round(b.reduceLeft(_ + _).lengthSquared*math.signum(ManifoldUtils.getDeterminant(b.map(_.coordinates))))
        println(r14)
        val v  = e14.get(r14, aminoacids(i).name, aminoacids(i + 1).name)
        println(v)
        v
      }
    ).reduceLeft (_ + _)
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

  def get_E_one(aminoacids : Seq[SimplifiedAminoAcid]) : Double = {
    ???
  }

  def get_E_pair(aminoacids : Seq[SimplifiedAminoAcid]) : Double = {
    ???
  }

  def get_E_tem(aminoacids : Seq[SimplifiedAminoAcid]) : Double = {
    ???
  }

  //TODO: we have pair of chains, that means we somehow should utilize that when we compute total energy
  def getEnergy(aminoacids : Array[SimplifiedAminoAcid]) : Double = {
    get_E_CA_trace(aminoacids) /*+
    get_E_H_bond(aminoacids) +
    get_E_rot(aminoacids) +
    get_E_SG_local(aminoacids) +
    get_E_one(aminoacids) +
    get_E_pair(aminoacids) +
    get_E_tem(aminoacids)*/
  }

}
