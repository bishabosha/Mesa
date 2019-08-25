package mesa.Prelude

primitive absurd : Void# ->. A#
primitive fix    : (A -> A) -> !A

data L |: R = Left L | Right R
data L# +: R# = InL[L#] | InR[R#]

fstL [p] : (A#, B#) ->. A# = case p of (a, _) =>. a
sndL [p] : (A#, B#) ->. B# = case p of (_, b) =>. b
fst p    : (A, B) -> A     = case p of (a, _) =>  a
snd p    : (A, B) -> B     = case p of (_, b) =>  b

id x   : a -> a    = x
idL[x] : A# ->. A# = x

dup x    : A -> (A, A)     = (x,x)
dupL [x] : A# ->. (A#, A#) = (x,x)

(<<<) g f a   : (B -> C) -> (A -> B) -> A -> C            = g (f a)
(<<!) g f [a] : (B# ->. C#) -> (A# ->. B#) -> (A# ->. C#) = g[f[a]]

uncurry f p    : (a -> b -> c) -> (a, b) -> c        = case p of (a,b) => f a b
uncurryL f [p] : (a -> b# ->. c#) -> !a *: b# ->. c# = let !a *: b = p in (f a)[b]

x ?= y: A -> A -> () = () -- type level assertion that x and y are the same type