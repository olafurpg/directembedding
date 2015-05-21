package ch.epfl.directembedding.transformers

import ch.epfl.directembedding.{ DirectEmbeddingModule, DirectEmbeddingUtils }

import language.experimental.macros

trait LiftLiteralTransformation extends DirectEmbeddingModule with DirectEmbeddingUtils {
  import c.universe._
  object LiftLiteralTransformer {
    def apply(toLift: List[Symbol])(tree: Tree): Tree = {
      val t = new LiftLiteralTransformer(toLift).transform(tree)
      log("lifted: " + t, 2)
      t
    }
  }

  class LiftLiteralTransformer(toLift: List[Symbol])
    extends Transformer {

    def genApply(t: List[Tree], name: TermName = TermName("lift")) =
      q"$configPath.$name(..$t)"

    def genApplyWithName(t: List[Tree], name: String = "lift") =
      genApply(t, TermName(name))

    def genApplyForType(t: List[Tree], tpe: Type) = {
      val name = tpe match {
        case t if t <:< c.typeOf[scala.Int] => "constColumnLift"
//        case t if t <:< c.typeOf[direct.Query] => "directQueryLift"
        case _                              => "lift"
      }

      genApply(t, name)
    }

    override def transform(tree: Tree): Tree = {
      tree match {
        case t @ Literal(Constant(_)) =>
          genApplyForType(List(t), tree.tpe)
        case t @ Ident(_) if toLift.contains(t.symbol) && !liftIgnore.exists(ignoreType => tree.tpe.widen <:< ignoreType) =>
          println(s"TPEEEEEE ${t.tpe.widen.widen}")
          genApply(List(Ident(TermName(t.name.decodedName.toString))))
        // the type associated with the identifier will remain if we don't that
        case t @ Ident(n) =>
          log("local variable: " + t, logLevel)
          Ident(n)
        case _ =>
          super.transform(tree)
      }
    }
  }

}
