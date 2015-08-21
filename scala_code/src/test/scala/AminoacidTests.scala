// import some geometry-related stuff
import org.scalatest.{Matchers, FlatSpec}

import ru.biocad.ig.common.structures.aminoacid._

class AminoacidsTests extends FlatSpec with Matchers {
  it should "construct simplified aminoacid representation from full-atom model correctly"
  it should "restore backbone atoms from alpha-carbons"
  it should "restore sidechains from backbone+united atom"
}
