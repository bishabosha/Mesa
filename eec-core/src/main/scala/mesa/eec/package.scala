package mesa.eec

import Trees.{Tree, ErasedTree}, Tree._


inline def (sc: => StringContext) eec (args: => Any*) <: ErasedTree = ${ Macros.eecImpl('sc, 'args) }

def effect[T](op: => T) : Tree[T] = App(Bang(), Lazy(() => op))
def succeed[T](value: T): Tree[T] = Pure(value)
