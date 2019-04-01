package eec
package util

import scala.annotation.tailrec

object StackMachine {
  import Compiler._
  import Program._

  type Stack     [T] = List[T]
  type Statement [T] = Stack[T] => Stack[T]

  opaque type Program[T] = List[Statement[T]]

  object Program {
    inline def of[T]: Program[T] = Nil

    def evalLeft[I,T](compiler: I => Statement[T])
                     (program: Program[T], i: I): Program[T] =
      compiler(i) :: program

    def (input: I) compile[I, O]
        (foldLeft: I => Program[O] => ((Program[O], I) => Program[O]) => Program[O])
        (compiler: I => Statement[O]): O =
      foldLeft(input)(of[O])(evalLeft(compiler)).unsafeInterpret

    private def (program: Program[T]) run[T]: Stack[T] = {
      @tailrec
      def inner[T](stack: Stack[T], program: Program[T]): Stack[T] =
        program match {
          case stat :: stats  => inner(stat(stack), stats)
          case Nil            => stack
        }
      inner(Nil, program)
    }

    def (program: Program[T]) unsafeInterpret [T] = program.run.head
  }
}