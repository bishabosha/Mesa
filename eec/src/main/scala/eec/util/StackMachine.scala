package eec
package util

object StackMachine {
  import annotation.tailrec
  import Program._

  type Stack[T]     = List[T]
  type Statement[T] = Stack[T] => Stack[T]

  object Compiler {
    def apply[I, T](compiler: I => Statement[T])
                   (program: Program[T], i: I): Program[T] =
      compiler(i) +: program
  }

  opaque type Program[T] = List[Statement[T]]

  object Program {
    
    inline def of[T]: Program[T] = Nil

    def (stat: Statement[T]) +: [T](program: Program[T]): Program[T] =
      stat :: program

    def (program: Program[T]) run[T]: Stack[T] = {
      @tailrec
      def inner[T](stack: Stack[T], program: Program[T]): Stack[T] =
        program match {
          case stat :: stats  => inner(stat(stack), stats)
          case Nil            => stack
        }
      inner(Nil, program)
    }
  }
}