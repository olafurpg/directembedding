package ch.epfl.directembedding.transformers

import ch.epfl.directembedding.{ DirectEmbeddingModule, DirectEmbeddingUtils, MacroModule }

class reifyAs(to: Any) extends scala.annotation.StaticAnnotation

trait ReifyAsEmbedding extends DirectEmbeddingModule with DirectEmbeddingUtils { self =>
  import c.universe._

  object ReifyAsTransformer extends (Tree => Tree) {
    def apply(tree: Tree) = {
      new ReifyAsTransformer(tree.pos).apply(tree)
    }
  }

  final class ReifyAsTransformer(pos: Position) extends Transformer {
    def apply(tree: Tree): Tree = transform(tree)

    def getReifyAnnotation(methodSym: Symbol): Option[Annotation] = methodSym.annotations.find(_.tree.tpe <:< c.typeOf[reifyAs])

    def reify(methodSym: Symbol, targs: List[Tree], args: List[Tree]): Tree = {
      val reifyAsAnnot = getReifyAnnotation(methodSym).getOrElse {
        c.abort(pos, s"$methodSym is not supported in $dslName")
      }

      val body :: _ = reifyAsAnnot.tree.children.tail

      (targs, args) match {
        case (Nil, Nil)    => body
        case (targs, Nil)  => q"${body}.apply[..$targs]"
        case (Nil, args)   => q"${body}.apply(..$args)"
        case (targs, args) => q"${body}.apply[..$targs](..$args)"
      }
    }

    def getInvokedSymbol(field: Select): Symbol = {
      field.symbol.annotations.find(_.tree.tpe <:< c.typeOf[reifyAs]).map { _ =>
        field.symbol
      }.getOrElse {
        // Value member compiled to private variable
        field.symbol.owner.info.members.find(x => x.name.toString.trim == field.symbol.name.toString.trim).getOrElse {
          throw new RuntimeException(s"Unable to get value member $field. ${showRaw(field)}")
        }
      }
    }

    /**
     * Returns transformed tree if tree is a class instance, None otherwise
     */
    def getSelf(x: Tree): Option[Tree] =
      if (x.symbol.isModule) None else Some(transform(x))

    /**
     * Convert curried function to uncurried function
     *
     * Example f(1)(2) => f(1, 2)
     *
     * @param t Tree to be uncurried
     * @param args List[Tree] arguments to prepend to uncurried argument list
     * @return Uncurried tree
     */
    def uncurry(t: Tree, args: List[Tree]): Tree = t match {
      case field @ Apply(x, y) =>
        uncurry(x, y ::: args)
      case _ =>
        Apply(t, args)
    }

    var indent: Int = 0
    var transformDebugLevel: Int = 3

    def log[T](e: T, level: Int = 0) = if (debugLevel > level) {
      print(" " * indent)
      self.log(s"e", level)
    }

    override def transform(tree: Tree): Tree = {
      log(s"transform():", transformDebugLevel)
      logTree(tree, transformDebugLevel + 1)
      indent += 2
      val result = tree match {
        case a @ Apply(Apply(_, _), _) =>
          log(s"Apply(Apply)", transformDebugLevel)
          val t = uncurry(a, Nil)
          transform(t)

        // f(args)
        case Apply(Select(New(newBody), _), cargs) =>
          log(s"Apply(Select(New)):", transformDebugLevel)
          val listArgs = newBody.tpe.typeArgs
          reify(newBody.symbol, listArgs.map(TypeTree(_)), cargs)

        // f(args)
        case Apply(field @ Select(x, _), args) =>
          log(s"Apply(Select):", transformDebugLevel)
          val invokedSymbol = getInvokedSymbol(field)
          val self = getSelf(x).toList
          reify(invokedSymbol, x.tpe.typeArgs.map(TypeTree(_)), self ::: args.map(transform(_)))

        // f[T](args)
        case Apply(TypeApply(field @ Select(x, _), targs), args) =>
          log(s"Apply(TypeApply):", transformDebugLevel)
          val invokedSymbol = getInvokedSymbol(field)
          val self = getSelf(x).toList
          reify(invokedSymbol, targs.map(transform(_)), self ::: args.map(transform(_)))

        case Apply(lhs, args) =>
          log(s"Apply() fallback:", transformDebugLevel)
          reify(lhs.symbol, Nil, args)

        // f[T]
        case TypeApply(field @ Select(x, _), targs) =>
          log(s"TypeApply", transformDebugLevel)
          val invokedSymbol = getInvokedSymbol(field)
          val self = getSelf(x).toList
          reify(invokedSymbol, x.tpe.typeArgs.map(TypeTree(_)) ::: targs.map(transform(_)), self)

        // obj.field
        case field @ Select(x, _) =>
          log(s"Select()", transformDebugLevel)
          val invokedSymbol = getInvokedSymbol(field)
          val self = getSelf(x).toList
          reify(invokedSymbol, x.tpe.typeArgs.map(TypeTree(_)), self)

        case _: Import => tree

        case _ =>
          log(s"other branch:", transformDebugLevel)
          super.transform(tree)
      }
      indent -= 2
      result
    }
  }
}
