package ch.epfl.directembedding.test

import ch.epfl.directembedding.DETransformer
import ch.epfl.directembedding.transformers.reifyAs
import ch.epfl.yinyang.EmbeddedControls

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context
package object linearalgebra {
  def la[T](block: T): T = macro implementations.liftRep[T]

  implicit def liftConstant[T](x: T): Const[T] = Const(x)

  object ConfigurationExample {
    @reifyAs(IF)
    def __ifThenElse[T](cond: Boolean, e1: T, e2: T): T = ???

    @reifyAs(NewVar)
    def __newVar(v: String): String = ???
  }

  def eval(ast: MatrixExpr): Matrix = {
    ast.eval
  }

  object implementations {
    def liftRep[T](c: Context)(block: c.Expr[T]): c.Expr[T] = {
      val config = "_root_.ch.epfl.directembedding.test.linearalgebra"
      DETransformer[c.type, T](c)(
        "example.dsl",
        s"$config.liftConstant",
        s"$config.ConfigurationExample",
        s"$config.eval",
        None,
        None)(block)
    }
  }
}
