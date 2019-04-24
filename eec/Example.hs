package eec.report.example

primitive x + y : Integer -> Integer -> Integer
primitive s1 ++ s2 : String -> String -> String
primitive debug x : A -> String
primitive putStrLn s : String -> !()
primitive readInteger : !Integer
primitive a == b : A -> A -> Boolean

liftD f x : (A -> B#) -> !A -> B# =
  let !y = x in f y

liftC f x : (A -> B) -> A -> !B =
  !(f x)

fst p: (A, B) -> A =
  case p of (a, _) => a

snd p: (A, B) -> B =
  case p of (_, b) => b

pairEq : Boolean = (fst (0,""), snd (0,"")) == (0,"")

ma >>= f : !A -> (A -> !B) -> !B =
  liftD f ma

f =<< ma : (A -> !B) -> !A -> !B =
  ma >>= f

makeState a [s] : a -> (S# ->. (!a *: S#)) = !a *: s

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
    let !y = (liftC succ) =<< readInteger in
    putStrLn ("paired: " ++ (debug (x, y)))