package ru.biocad.ig.alascan.constants

import ru.biocad.ig.common.io.pdb.PDBAtomInfo
import ru.biocad.ig.common.structures.aminoacid.SimplifiedAminoacid
import ru.biocad.ig.common.structures.geometry.GeometryVector

import com.typesafe.scalalogging.slf4j.LazyLogging

/** Storage, which contains information about aminoacid fragment's positions collected from Protein Data Bank, based on local topology
  *
  * @constructor creates object from given collected information
  * @param data contains Map of AminoacidFragment objects, collected for given aminoacid (first level Map key) and set of distance-based parameters (next levels of Map's keys)
  * @param meshSize collected data and int keys in Map are discretized based on this parameter
  * @param threshold ??? i don't remember //TODO: remember!
  */
case class AminoacidLibrary[T <: AminoacidFragment](
  val data : Map[String, Map[Int, Map[Int, Map[Int, T]]]],
  val meshSize : Double = 0.3,
  val threshold : Double = 0.0)(implicit m: scala.reflect.Manifest[T]) extends LazyLogging {
    /** Finds corresponding AminoacidFragment in database (based on d1, d2 and d3 parameters) and returns it
      * @param aminoacid current library fragments are aminoacid-specific, so this param correspongs to first level of grouping - aminoacid name
      * @param d1 distance-based grouping parameter in global coordinates (gets converted to discrete mesh units inside method call).
      * @param d2 distance-based grouping parameter in global coordinates (gets converted to discrete mesh units inside method call).
      * @param d3 distance-based grouping parameter in global coordinates (gets converted to discrete mesh units inside method call).
      * @return corresponding AminoacidFragment in current library or empty object of that type
      */
    def restoreAminoacidInfo(aminoacid : String,
            d1 : Double, d2 : Double, d3 : Double) : Option[T] = {
      val i1 = math.round(d1 / meshSize).toInt
      val i2 = math.round(d2 / meshSize).toInt
      val i3 = math.round(d3 / meshSize).toInt
      val m0 = data.get(aminoacid) match {
        case Some(x) => x
        case None => Map[Int, Map[Int, Map[Int, T]]]()
      }

      val m1 : Map[Int, Map[Int, T]] = m0.get(i1) match {
        case Some(v) => v
        case None => {
          if (m0.nonEmpty) {
            m0.minBy(x => math.abs(x._1 - i1))._2
          }
          else Map[Int, Map[Int, T]]()
        }

      }
      val m2 : Map[Int, T] = m1.get(i2) match {
        case Some(v) => v
        case None => {
          if (m1.nonEmpty){
            m1.minBy(x => math.abs(x._1 - i2))._2
          }
          else Map[Int, T]()
        }
      }
      val m3 : Option[T] = m2.get(i3) match {
        case None => {
          if (m2.nonEmpty) {
            Some(m2.minBy(x => math.abs(x._1 - i3))._2)
          }
          else None
        }
        case x => x
      }
      m3
      //TODO: add existance check and lookup for nearest point - or at least smth located near
    }

    /** Gets fragment of full-atom representation for given simplified aminoacid based on local geometry params (d1, d2, d3) and local coordinate system.
      * @param aminoacid simplified object, for which portion of full-atom representation gets retrieved
      * @param d1 distance-based clustering parameter
      * @param d2 distance-based clustering parameter
      * @param d2 distance-based clustering parameter
      * @param x local coordinate system's x axis vector
      * @param y local coordinate system's y axis vector
      * @param z local coordinate system's z axis vector
      * @return list of PDBAtomInfo for given fragment with updated coordinates
      */
    def restorePDBInfo(aminoacid : SimplifiedAminoacid,
            d1 : Double, d2 : Double, d3 : Double,
            x : GeometryVector, y : GeometryVector, z : GeometryVector,
            atomsMap : Map[String, PDBAtomInfo]) : Seq[PDBAtomInfo] = {
        //val (d1, d2, d3) = AminoacidUtils.getDistances(a1.ca, a2.ca, a3.ca, a4.ca)
        restoreAminoacidInfo(aminoacid.name, d1, d2, d3) match {
          case Some(coordinatesMap) => coordinatesMap.getPDBAtomInfo(aminoacid, x, y, z, atomsMap)
          case None => Seq() // TODO: add some check to avoid problems
        }
        //val (x, y, z) = AminoacidUtils.getLocalCoordinateSystem(a1.ca, a2.ca, a3.ca, a4.ca)
    }

    def restoreCoordinates(aminoacid : SimplifiedAminoacid,
            d1 : Double, d2 : Double, d3 : Double,
            x : GeometryVector, y : GeometryVector, z : GeometryVector
          ) : Map[String, GeometryVector] = {
        //val (d1, d2, d3) = AminoacidUtils.getDistances(a1.ca, a2.ca, a3.ca, a4.ca)
        restoreAminoacidInfo(aminoacid.name, d1, d2, d3) match {
          case Some(coordinatesMap) => coordinatesMap.getCoordinatesMap(aminoacid, x, y, z)
          case None => Map[String, GeometryVector]()
        }
        //val (x, y, z) = AminoacidUtils.getLocalCoordinateSystem(a1.ca, a2.ca, a3.ca, a4.ca)

    }

}
