package ch.epfl.directembedding.test

import ch.epfl.directembedding.test.Typecheck._
import ch.epfl.directembedding.test.linearalgebra._
import org.scalatest.{ FlatSpec, ShouldMatchers }

class LinearAlgebraSpec extends FlatSpec with ShouldMatchers {

  def testReify(body: Collector => Exp[_]): Seq[Exp[_]] = {
    implicit val collec: Collector = new CollectClass()
    intercept[scala.NotImplementedError] {
      body(collec)
    }
    collec.get
  }
  "dsl" should "lift free variables in block" in {
    testReify(implicit collec =>
      la {
        true
      }) should be(List(Const(true)))
  }

}
