package ch.epfl.directembedding.test

import ch.epfl.directembedding.test.Typecheck._
import ch.epfl.directembedding.test.linearalgebra._
import org.scalatest.{ FlatSpec, ShouldMatchers }

class LinearAlgebraSpec extends FlatSpec with ShouldMatchers {

  "linear algebra" should "lift a single matrix" in {
    la {
      new Matrix(1, 2)
    } should be(Matrix(1, 2))
  }

  "linear algebra" should "work with matrix multiplication" in {
    la {
      val a = new Matrix(1, 2)
      val b = new Matrix(2, 1)
      a * b
    } should be(Matrix(1, 1))
  }

}
