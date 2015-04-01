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
      val c = new Matrix(3, 6)
      val b = new Matrix(2, 5)
      // Polymorphic embedding
      // Finally tagless
      a * b
    } should be(Matrix(1, 5))
  }

  "linear algebra" should "work wit??? matrix multiplication" in {
    la {
      val a = new Matrix(1, 2)
      var c = new Matrix(3, 6)
      val b = new Matrix(2, 5)
      a * b
    } should be(Matrix(1, 5))
  }

}
