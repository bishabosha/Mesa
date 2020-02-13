package mesa.eec

import Trees.{Tree, ErasedTree}, Tree._


inline def (inline sc: StringContext) eec (inline args: Any*) <: ErasedTree = ${ Macros.eecImpl('sc, 'args) }

def effect[T](op: => T) : Tree[T] = Bang(Lazy(() => op))
def succeed[T](value: T): Tree[T] = Pure(value)
