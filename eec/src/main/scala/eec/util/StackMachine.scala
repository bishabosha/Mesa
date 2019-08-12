package eec.util

import annotation.tailrec

object StackMachine {
  export opaques.{Program, Stack, stackInit, stack}

  type Statement[T] = given opaques.Stack[T] => List[T]

  trait Interpretable[I] {
    def (i: I) interpret[O] (z: O)(f: (O, I) => O): O
  }

  object opaques {
    opaque type Program[T] = List[Statement[T]]
    opaque type Stack[T]   = List[T]

    def stackInit[T]: Program[T] = Nil
    def stack[T] given Stack[T]: List[T] = the[List[T]]

    given {

      def (t: Statement[T]) +: [T](p: Program[T]): Program[T] = t :: p

      def (program: Program[T]) unsafeInterpret[T]: T = run(Nil, program).head

      @tailrec private def run[T](stack: Stack[T], program: Program[T]): List[T] = program match {
        case stat :: stats => run(stat given stack, stats)
        case Nil           => stack
      }
    }
  }

  object Program {
    def (input: I) compile[I: Interpretable, O]
        (compiler: I => Statement[O]): O =
      input.interpret(stackInit[O])((p, i) => compiler(i) +: p).unsafeInterpret
  }
}