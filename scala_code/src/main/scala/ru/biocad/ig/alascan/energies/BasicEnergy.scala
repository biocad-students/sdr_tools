package ru.biocad.ig.alascan.energies

import ru.biocad.ig.common.structures.aminoacid.SimplifiedChain

trait BasicEnergy {
  def get(aminoacids : SimplifiedChain) : Double = ???
}
