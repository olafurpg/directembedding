package ch.epfl.directembedding

import org.scalatest.{ FlatSpec, ShouldMatchers }

class InlineSpec extends FlatSpec with ShouldMatchers {

  "lift" should "work object fields" in {
    intercept[scala.NotImplementedError] {
      lift {
        ObjectExample.valDef
      }
    }
  }
  it should "work with object methods without arguments and type arguments" in {
    intercept[scala.NotImplementedError] {
      lift {
        ObjectExample.noArgs
      }
    }
  }

  it should "work with object methods with just arguments" in {
    intercept[scala.NotImplementedError] {
      lift {
        ObjectExample.justArgs(1)
      }
    }
  }

}
