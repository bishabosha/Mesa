package mesa.compiler.core

object Constants {

  opaque type Constant = ConstantScala

  type ConstantScala = Boolean | BigDecimal | BigInt | String | Char

  object Constant {
    def BigDecConstant(bd: BigDecimal): Constant = bd
    def BigIntConstant(bi: BigInt): Constant = bi
    def StringConstant(str: String): Constant = str
    def CharConstant(c: Char): Constant = c
    val True: Constant  = true
    val False: Constant = false

    extension (c: Constant) def asScala: ConstantScala = c
  }
}