package eec.compiler.core

object Constants {
  enum Constant derives Eql {
    case StringConstant(str: String)
    case CharConstant(chr: Char)
    // case IntConstant(i: Int)
    // case LongConstant(l: Long)
    // case FloatConstant(f: Float)
    // case DoubleConstant(d: Double)
    case BigIntConstant(bi: BigInt)
    case BigDecConstant(bd: BigDecimal)
    case BooleanConstant(z: Boolean)
  }
}