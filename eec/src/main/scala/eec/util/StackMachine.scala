package eec
package util

object StackMachine {
  import annotation.tailrec
  import Program._

  type Stack     [T] = List[T]
  type Statement [T] = Stack[T] => Stack[T]

  opaque type Compiler[I,T] = I => Statement[T]

  object Compiler {
    def apply[I, T](compiler: I => Statement[T]): Compiler[I, T]   = compiler
    def (compiler: Compiler[I, T]) unseal[I, T]: I => Statement[T] = compiler
  }

  opaque type Program[T] = List[Statement[T]]

  object Program {
    import Compiler._

    inline def of[T]: Program[T] = Nil

    def evalLeft[I,T](compiler: Compiler[I, T])
                     (program: Program[T], i: I): Program[T] =
      compiler.unseal(i) :: program

    def evalRight[I,T](compiler: Compiler[I, T])
                      (i: I, program: Program[T]): Program[T] =
      compiler.unseal(i) :: program

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
    def (program: Program[T]) interpret       [T] = program.run.headOption
  }
}