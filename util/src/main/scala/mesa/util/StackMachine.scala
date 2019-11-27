package mesa.util

import annotation.tailrec

object StackMachine {
  export opaques.{Program, Stack, stackInit, stack}

  type Statement[T] = (given opaques.Stack[T]) => List[T]

  trait Interpretable[I] {
    def [O](i: I) interpret (z: O)(f: (O, I) => O): O
  }

  trait InterpretableK[F[?]] {
    def [T, O](i: F[T]) interpretK(z: O)(f: [t] => (O, F[t]) => O): O
  }

  object opaques {
    opaque type Program[T] = List[Statement[T]]
    opaque type Stack[T]   = List[T]

    def stackInit[T]: Program[T] = Nil
    def stack[T](given Stack[T]): List[T] = summon[List[T]]

    given {

      def [T](t: Statement[T]) +: (p: Program[T]): Program[T] = t :: p

      def [T](program: Program[T]) unsafeInterpret: T = run(Nil, program).head

      @tailrec private def run[T](stack: Stack[T], program: Program[T]): List[T] = program match {
        case stat :: stats => run(stat(given stack), stats)
        case Nil           => stack
      }
    }
  }

  object Program {
    def [I: Interpretable, O](input: I) compile(step: I => Statement[O]): O =
      input.interpret[Program[O]](stackInit)((p, i) => step(i) +: p).unsafeInterpret

    def [F[?]: InterpretableK, T, O](input: F[T]) compile(step: F[Any] => Statement[O]): O =
      input.interpretK[T, Program[O]](stackInit)(step.toCompiler[F,O]).unsafeInterpret

    def [F[?], O](step: F[Any] => Statement[O]) toCompiler =
      ([t] => (p: Program[O], i: F[t]) => step(i.asInstanceOf[F[Any]]) +: p)
  }
}
