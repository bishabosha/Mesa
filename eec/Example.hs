package eec.report.example

primitive (+) : Integer -> Integer -> Integer
primitive (++) : String -> String -> String
primitive debug : A -> String
primitive putStrLn : String -> !()
primitive readInteger : !Integer
primitive (==) : A -> A -> Boolean

liftC f x : (A -> B) -> A -> !B =
  !(f x)

fst p: (A, B) -> A =
  case p of (a, _) => a

snd p: (A, B) -> B =
  case p of (_, b) => b

pairEq : Boolean = (fst (0,""), snd (0,"")) == (0,"")

x >>= f : !A -> (A -> !B) -> !B = let !y = x in f y

fmap f x : (A -> B) -> !A -> !B =
  x >>= (liftC f)

makeState a [s] : a -> (s# ->. (!a *: s#)) = !a *: s

-- x >>= f : !a -> (a -> !b) -> !b = let !y = x in f y
-- !0 >>= (\(i: Integer) => !(i + 1))
-- \(f: a -> b -> c)(p: (a,b)) => case p of (a,b) => f a b

g <<< f : (B -> C) -> (A -> B) -> A -> C = \(a: A) => g (f a)
-- \(p: (a#,b#)) =>. case p of (a,_) =>. a
{-
eec> :def g <<< f : (B -> C) -> (A -> B) -> A -> C = \(a: A) => g (f a)
defined <<< : (B -> C) -> (A -> B) -> A -> C

eec> :t (<<<)
(<B:1> -> <C:2>) -> (<A:3> -> <B:1>) -> <A:3> -> <C:2>

-}

id x: a -> a = x

idL[x] : A# ->. A# = x

succ x : Integer -> Integer =
  x + 1

isZero x : Integer -> Boolean =
  case x of
    0 => True
    _ => False

main : !() =
  let !x = readInteger in
  if isZero x then
    putStrLn "Entered Zero"
  else
    let !y = readInteger >>= (liftC succ) in
    putStrLn ("paired: " ++ (debug (x, y)))