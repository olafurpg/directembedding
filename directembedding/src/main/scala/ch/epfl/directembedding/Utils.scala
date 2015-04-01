package ch.epfl.directembedding

import ch.epfl.yinyang.TransformationUtils

import scala.reflect.macros.blackbox.Context

// Inspired by https://github.com/scala-yinyang/scala-yinyang/blob/16732662470992e10a7ae479d8be5419f13d3654/components/core/src/Utils.scala

trait MacroModule {
  type Ctx <: Context
  val c: Ctx
}

trait DirectEmbeddingModule extends MacroModule {
  val dslName: String
  val dslEndpointMethod: String
  val virtualizationConfig: String
  val liftMethodName: String
  val logLevel: Int
}

trait DirectEmbeddingUtils extends MacroModule with TransformationUtils {
  import c.universe._

  def logTree(t: Tree, level: Int = 0) = {
    log(s"$t", level)
    log(showRaw(t, printTypes = true), level + 1)
  }
}
