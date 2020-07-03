package mesa.util

import annotation.tailrec

object StackMachine:
  export opaques.{Program, Stack, stackInit, stack}

  type Statement[T] = opaques.Stack[T] ?=> List[T]

  trait Interpretable[I]:
    extension [O](i: I) def interpret (z: O)(f: (O, I) => O): O

  trait InterpretableK[F[_]]:
    extension [T, O](i: F[T]) def interpretK(z: O)(f: [t] => (O, F[t]) => O): O

  object opaques:

    opaque type Program[T] = List[Statement[T]]
    opaque type Stack[T]   = List[T]

    def stackInit[T]: Program[T] = Nil
    def stack[T](using Stack[T]): List[T] = summon[List[T]]

    given Object:

      def [T](t: Statement[T]) +: (p: Program[T]): Program[T] = t :: p

      def [T](program: Program[T]) unsafeInterpret: T = run(Nil, program).head

      @tailrec private def run[T](stack: Stack[T], program: Program[T]): List[T] = program match
        case stat :: stats => run(stat(using stack), stats)
        case Nil           => stack

    end given

  end opaques

  object Program:

    def [I: Interpretable, O](input: I) compile(step: I => Statement[O]): O =
      input.interpret[Program[O]](stackInit)((p, i) => step(i) +: p).unsafeInterpret

    def [F[_]: InterpretableK, T, O](input: F[T]) compile(step: F[Any] => Statement[O]): O =
      input.interpretK[T, Program[O]](stackInit)(step.toCompiler[F,O]).unsafeInterpret

    def [F[_], O](step: F[Any] => Statement[O]) toCompiler = [t] =>
      (p: Program[O], i: F[t]) => step(i.asInstanceOf[F[Any]]) +: p

  end Program

end StackMachine
