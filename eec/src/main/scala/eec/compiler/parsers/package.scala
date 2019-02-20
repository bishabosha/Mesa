package eec
package compiler
package parsing

import ast.Trees._
import Parsers._
import Parsers.TreeParsers._
import error.CompilerErrors._
import core.Contexts._

val parseEEC: String => Contextual[Checked[Tree]] =
  eecParser toTreeParser fromTranslationUnit

val parseExpr: String => Contextual[Checked[Tree]] =
  exprParser toTreeParser fromExpr

val parseType: String => Contextual[Checked[Tree]] =
  typeParser toTreeParser fromType