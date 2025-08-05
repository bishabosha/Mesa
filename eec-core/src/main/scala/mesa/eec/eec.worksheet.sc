import mesa.eec._
import mesa.util.Show

def puts[A](a: A)(using Show[A]) =
  println(a.show)

given Show[String] = identity

puts(eec"*")
// puts(eec"""(\x.\y.y) (\x.y) *""")
// puts(eec"let !x *: z be s in inr z")
puts(eec"let !y be x in *")
puts(eec"!a")
// puts(eec"?v")
puts(eec"a")
// puts(eec"!f g")
// puts(eec"!(f g)")
puts(eec"fst (a, b)")
// puts(eec"snd (*, (a, b))")
// puts(eec"inl *")
// puts(eec"inr *")
puts(eec"!a *: b")
// puts(eec"""!a *: ((\x.x) g)""")
// puts(eec"f (a b) (c d e)")
// puts(eec"""case inl z of {inl l.(\x.\y.*) a b; inr r.*}""")
// puts(eec"""(\x.x) (\x.x)""")
// puts(eec"""(^\x.x)[${23}]""")

val I = eec"""\x.x"""
val K = eec"""\x.\y.x"""
// val S = eec"""\f.\g.\x.f x (g x)"""

// val Bind = eec"""\f.\x.let !y be x in f y"""

puts(I)
puts(K)
// puts(S)

// puts(Bind)
