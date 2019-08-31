package mesa.eec

import Trees.Tree

inline def (sc: => StringContext) eec (args: => Any*) <: Tree[?] = ${ Macros.eecImpl('sc, 'args) }