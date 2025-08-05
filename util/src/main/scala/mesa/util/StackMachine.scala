package mesa.util

import annotation.tailrec

object StackMachine:
  export opaques.{Program, Stack, stackInit, stack}

  type Statement[T] = (opaques.Stack[T]) ?=> List[T]

  trait Interpretable[I]:
    extension (i: I) def interpret [O](z: O)(f: (O, I) => O): O

  trait InterpretableK[F[_]]:
    extension [T](i: F[T]) def interpretK[O](z: O)(f: [T] => (O, F[T]) => O): O

  object opaques :

    opaque type Program[T] = List[Statement[T]]
    opaque type Stack[T]   = List[T]

    def stackInit[T]: Program[T] = Nil
    def stack[T](using Stack[T]): List[T] = summon[List[T]]

    given Object with

      extension [T](t: Statement[T]) def +: (p: Program[T]): Program[T] = t :: p

      extension [T](program: Program[T]) def unsafeInterpret: T = run(Nil, program).head

      @tailrec private def run[T](stack: Stack[T], program: Program[T]): List[T] = program match
        case stat :: stats => run(stat(using stack), stats)
        case Nil           => stack

    end given

  end opaques

  object Program :

    extension [I: Interpretable](input: I) def compile[O](step: I => Statement[O]): O =
      input.interpret[Program[O]](stackInit)((p, i) => step(i) +: p).unsafeInterpret

    extension [F[_]: InterpretableK, T](input: F[T]) def compile[O](step: [U] => F[U] => Statement[O]): O =
      input.interpretK[Program[O]](stackInit)(toCompiler[F, O](step)).unsafeInterpret

    def toCompiler[F[_], O](step: [U] => F[U] => Statement[O]): [T] => (Program[O], F[T]) => Program[O] =
      [T] => (p: Program[O], i: F[T]) => step[T](i) +: p

  end Program

end StackMachine
