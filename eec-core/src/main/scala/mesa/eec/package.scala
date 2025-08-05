package mesa.eec

import Trees.Tree

extension (inline sc: StringContext) transparent inline def eec (inline args: Any*): Tree[?] =
  ${ Macros.eecImpl('sc, 'args) }