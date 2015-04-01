package ch.epfl.directembedding.transformers

import ch.epfl.directembedding.{ DirectEmbeddingModule, DirectEmbeddingUtils }

trait EndpointTransformation extends DirectEmbeddingModule with DirectEmbeddingUtils {
  import c.universe._

  def EndpointTransformer = new (Tree => Tree) {

    def apply(tree: Tree): Tree = {
      q"""
        ${c.parse(dslEndpointMethod)}($tree)
       """
    }
  }

}
