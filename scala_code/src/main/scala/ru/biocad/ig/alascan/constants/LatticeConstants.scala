package ru.biocad.ig.alascan.constants

import spray.json._
import DefaultJsonProtocol._
import ru.biocad.ig.common.structures.aminoacid.SimplifiedChain
import ru.biocad.ig.common.structures.geometry.Lattice
import com.typesafe.scalalogging.slf4j.LazyLogging
import ru.biocad.ig.alascan.energies._

case class Restrictions(min : Double, max : Double) {
  def check(value : Double) : Boolean = value >= min && value <= max
}

object RestrictionsJsonProtocol extends DefaultJsonProtocol {
  implicit val restrictionsJsonFormat = jsonFormat2(Restrictions)
}
import RestrictionsJsonProtocol._

/**coarse lattice class. currently implemented as a singleton object with a lot of constant values.
Later should refactor to class with file parameters loading, thus making creation of different lattice objects possible
*/
case class LatticeConstants(val name : String,
        val meshSize : Double,
        val angleRestrictions : Restrictions,
        val basicVectorsFileName : String,
        val caMinDistance : Double,
        val eH : Double,
        val eHH : Double,
        val hBondR : Restrictions,
        val hBondAmax : Double,
        val contactCutoff : Double,
        val energyTerms : Map[String, Double]) {

    def distanceConditionForHBonds(r: Double) : Boolean = hBondR.check(r)
    def checkAngleRestrictions(angle : Double) : Boolean = angleRestrictions.check(angle)

    /** Builds energy function from `energyTerms` constructor parameter.
      *
      * @param lattice Lattice object, which gets sent to energy terms constructors
      * @return energy function with weighted parameters, which can be used to value current amino acid chain properties
      *
      * This method's logic is similar to getCofactors method in [[ru.biocad.ig.common.algorithms.geometry.ManifoldUtils]]
      * it finds energy classes given in json hash with settings,  for each of them calls constructor with 1 argument of type Lattice,
      * and then for resulting objects constructs function with weighted arguments, each argument is a result of call to get method of corresponding energy term
      * This method is a way to build flexible energy function, with flexible weights and components, with no need to recompile the whole project when some term or weight has changed.
      *
      * call sample:
      * {{{
      *  val getEnergy = buildEnergyFunction(lattice)
      *  //then call many times (and notice, that we saved result to `val`, not `def`):
      *  val result = getEnergy(chain)
      * }}}
      *
      */
    def buildEnergyFunction(lattice : Lattice) : (SimplifiedChain) => Double = {
      val energyPartials = energyTerms.map({case (energyTermClassName, weight) =>
        (Class.forName(energyTermClassName).getConstructor(classOf[Lattice]).newInstance(lattice).asInstanceOf[BasicEnergy], weight)
      })
      println("constructing energy function")
      (chain : SimplifiedChain) => energyPartials.map({case (k, w) => k.get(chain) * w}).sum
    }
}


object LatticeConstantsJsonProtocol extends DefaultJsonProtocol {
  implicit val latticeConstantsJsonFormat = jsonFormat11(LatticeConstants)
}

import LatticeConstantsJsonProtocol._
