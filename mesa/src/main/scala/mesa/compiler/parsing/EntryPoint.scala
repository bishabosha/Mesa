package mesa.compiler.parsing

import mesa.compiler.ast.Trees.Tree
import mesa.compiler.error.CompilerErrors.Lifted
import mesa.compiler.core.Contexts.IdReader

val parseMesa: String => IdReader[Lifted[Tree]] =
  mesaParser `toTreeParser` fromTranslationUnit

val parseDef: String => IdReader[Lifted[Tree]] =
  statParser `toTreeParser` fromStatAsTop

val parseExpr: String => IdReader[Lifted[Tree]] =
  exprParser `toTreeParser` fromExprAsTop