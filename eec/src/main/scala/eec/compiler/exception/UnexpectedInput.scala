package eec
package compiler
package exception

case class UnexpectedInput(msg: String) extends Exception(msg)