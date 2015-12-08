package ru.biocad.ig.common.structures.geometry


/**
  * the following class iterates through given simple structures of the Simplex class
  */
class GeometryVectorIterator(input_sequence : Seq[GeometryVector]) extends Iterator[(GeometryVector, Seq[GeometryVector])] {
  var prefix : Seq[GeometryVector] = Seq()
  var tailSequence : Seq[GeometryVector] = input_sequence

  def hasNext : Boolean = !tailSequence.isEmpty

  def next() : (GeometryVector, Seq[GeometryVector]) = {
    if (hasNext) {
      val element : GeometryVector = tailSequence.head
      tailSequence = tailSequence.tail
      val allExceptElement : Seq[GeometryVector] = tailSequence ++ prefix
      prefix = prefix ++ Seq(element)
      (element, allExceptElement)
    } else {
      null
    }
  }
}
