package eec
package util

object Programs {
  import annotation.tailrec

  type Stack[T]     = List[T]
  type Statement[T] = Stack[T] => Stack[T]

  opaque type Program[T] = List[Statement[T]]

  object Program {
    
    inline def of[T]: Program[T] = Nil

    object Compiler {
      def apply[I, T](compiler: I => Statement[T])
                     (program: Program[T], i: I): Program[T] =
        compiler(i) :: program
    }

    def (program: Program[T]) run[T]: Stack[T] = {
      @tailrec
      def inner[T](stack: Stack[T], program: Program[T]): Stack[T] =
        program match {
          case stat :: stats  => inner(stat(stack), stats)
          case Nil            => stack
        }
      inner(Nil, program)
    }

    def (program: Program[T]) unsafeInterpret[T]: T =
      program.run.head
  }
}