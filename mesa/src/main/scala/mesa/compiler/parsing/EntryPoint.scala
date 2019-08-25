package mesa
package compiler
package parsing

import ast.Trees.Tree
import error.CompilerErrors.Lifted
import core.Contexts.IdReader

val parseMesa: String => IdReader[Lifted[Tree]] =
  mesaParser `toTreeParser` fromTranslationUnit

val parseDef: String => IdReader[Lifted[Tree]] =
  statParser `toTreeParser` fromStatAsTop

val parseExpr: String => IdReader[Lifted[Tree]] =
  exprParser `toTreeParser` fromExprAsTop