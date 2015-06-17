package ru.biocad.ig.common.algorithms.geometry

import ru.biocad.ig.common.structures.geometry._

class Ridge(val vertices : Set[GeometryVector]){
}

class Facet (val vertices : Set[GeometryVector]){
  var neighbours = collection.mutable.Map[Ridge, Facet]()
}
