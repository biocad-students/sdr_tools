package ru.biocad.ig.common.io.pdb

import scala.io.Source
import ru.biocad.ig.common.io.common.SourceReader


class PDBReader(source : Source) {
  private val reader = new SourceReader(source)
  private var nextAtomString : String = skipToNext()

  def this(filename : String) = this(Source.fromFile(filename))

  def hasNext : Boolean = nextAtomString != null

  def next() : Option[PDBAtomInfo] = {
    if (hasNext) {
      val ai : PDBAtomInfo =  PDBAtomInfo(nextAtomString)
      nextAtomString = skipToNext()
      Some(ai)
    } else {
      None
    }
  }

  def close() : Unit = reader.close()

  private def skipToNext() : String = {
    var line : String = ""
    reader.synchronized {
      line = reader.readLine()
      while (line != null && line.substring(0, 4) != "ATOM") {
        line = reader.readLine()
      }
    }
    line
  }
}
