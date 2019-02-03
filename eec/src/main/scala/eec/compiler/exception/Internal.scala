package eec
package compiler
package exception

class Internal(e: Exception) extends Exception(e.getMessage, e)