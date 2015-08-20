package ru.biocad.ig.common.io.common

import scala.io.Source

class SourceReader(source : Source) {
  var lines = source.getLines()

  def readLine() : String = {
    if (lines.hasNext) {
      lines.next()
    }
    else {
      null
    }
  }

  def close() : Unit = {
    source.close()
  }
}
