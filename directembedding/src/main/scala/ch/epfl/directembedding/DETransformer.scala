package ch.epfl.directembedding

import ch.epfl.directembedding.transformers._
import ch.epfl.yinyang.analysis.FreeIdentAnalysis
import ch.epfl.yinyang.transformers.{ LanguageVirtualization, NullPreProcessing, NullPostProcessing, PreProcessing, PostProcessing }

import scala.reflect.macros.{ TypecheckException, blackbox }
import scala.reflect.runtime.universe._

object DETransformer {

  val defaultOptions: Map[String, Boolean] = Map(
    "virtualizeFunctions" -> true,
    "embedFunctions" -> false,
    "embedFunctions" -> false,
    "virtualizeVal" -> true
  )

  def apply[C <: blackbox.Context, T, D <: DslConfig](c: C)(
    _dslName: String,
    _options: Map[String, Boolean],
    _typeMap: Map[c.universe.Type, c.universe.Type],
    preProcessing: Option[PreProcessing[c.type]],
    postProcessing: Option[PostProcessing[c.type]],
    debug: Boolean = false)(implicit tag: WeakTypeTag[D]): DETransformer[c.type, T] = {
    import c.universe._

    val configName = tag.tpe.typeSymbol.fullName
    val dslConfig: Tree = c.parse(configName)
    val options = _options withDefault(defaultOptions)

    new DETransformer[c.type, T](c) {
      val postProcessor = postProcessing.getOrElse(new NullPostProcessing[c.type](c))
      val preProcessor = preProcessing.getOrElse(new NullPreProcessing[c.type](c))

      val dslName: String = _dslName
      val configPath: Tree = dslConfig
      // Options
      override val failCompilation: Boolean = options("failCompilation")
      override val virtualizeFunctions: Boolean = options("virtualizeFunctions")
      override val virtualizeVal: Boolean = options("virtualizeVal")
      override val embedFunctions: Boolean = options("embedFunctions")

      override val typeMap: Map[String, Type] = _typeMap.map {
        case (k, v) =>
          (k.typeSymbol.fullName, v)
      }
      override val debugLevel = if (debug) 2 else 0
      val logLevel: Int = 0
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
  val postProcessor: PostProcessing[c.type]
  val preProcessor: PreProcessing[c.type]
  import postProcessor._
  import preProcessor._

  def apply[T](block: c.Expr[T]): c.Expr[T] = {

    val tree = block.tree

    logStarred("Before Transformation", logLevel, tree)

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

    logStarred("After Transformation", logLevel, transformed)

    c.Expr[T](transformed)
  }

}
