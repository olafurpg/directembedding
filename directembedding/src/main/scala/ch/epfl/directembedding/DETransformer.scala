package ch.epfl.directembedding

import ch.epfl.directembedding.transformers.{ LiftLiteralTransformation, EndpointTransformation, DSLVirtualization, ReifyAsEmbedding }
import ch.epfl.yinyang.analysis.FreeIdentAnalysis
import ch.epfl.yinyang.transformers.{ LanguageVirtualization, NullPreProcessing, NullPostProcessing, PreProcessing, PostProcessing }

import scala.reflect.macros.blackbox

object DETransformer {
  def apply[C <: blackbox.Context, T](c: C)(
    _dslName: String,
    _liftMethodName: String,
    _virtualizationConfig: String,
    _dslEndpointMethod: String,
    postProcessing: Option[PostProcessing[c.type]],
    preProcessing: Option[PreProcessing[c.type]]): DETransformer[c.type, T] = {
    import c.universe.Tree
    new DETransformer[c.type, T](c) {
      val postProcessor = postProcessing.getOrElse(new NullPostProcessing[c.type](c))
      val preProcessor = preProcessing.getOrElse(new NullPreProcessing[c.type](c))
      val liftMethodName: String = _liftMethodName
      val virtualizationConfig: String = _virtualizationConfig
      val dslName: String = _dslName
      val dslEndpointMethod: String = _dslEndpointMethod
    }
  }
}

abstract class DETransformer[C <: blackbox.Context, T](val c: C)
  extends DirectEmbeddingModule
  with ReifyAsEmbedding
  with LanguageVirtualization
  with DSLVirtualization
  with FreeIdentAnalysis
  with LiftLiteralTransformation
  with EndpointTransformation {
  type Ctx = C
  import c.universe._
  val debugLevel: Int = 1
  val failCompilation: Boolean = false
  val virtualizeFunctions: Boolean = false
  val virtualizeVal: Boolean = false
  val postProcessor: PostProcessing[c.type]
  val preProcessor: PreProcessing[c.type]
  import postProcessor._
  import preProcessor._

  private def log(where: String)(t: Tree): Tree = {
    super.log(where)
    logTree(t)
    t
  }

  def apply[T](block: c.Expr[T]): c.Expr[T] = {

    val tree = block.tree

    val captured = freeVariables(tree)
    val toLifted = captured.map(_.symbol)

    val transformed: Tree = {
      (PreProcess andThen
        (x => c.untypecheck(x)) andThen
        (x => VirtualizationTransformer(x)._1) andThen
        DSLVirtualizer andThen
        ReifyAsTransformer andThen
        LiftLiteralTransformer(toLifted) andThen
        EndpointTransformer andThen
        (x => c.untypecheck(x)) andThen
        PostProcess)(tree)
    }

    c.Expr[T](transformed)
  }

}
