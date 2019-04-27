package eec.report.example

-- Please import the Prelude by using the `-p` flag

primitive (+) : Integer -> Integer -> Integer
primitive (++) : String -> String -> String
primitive debug : A -> String
primitive putStrLn : String -> !()
primitive readInteger : !Integer
primitive (==) : A -> A -> Boolean

liftC f x : (A -> B) -> A -> !B =
  !(f x)

pairEq : Boolean = (fst (0,""), snd (0,"")) == (0,"")

x >>= f : !A -> (A -> !B) -> !B = let !y = x in f y

fmap f x : (A -> B) -> !A -> !B =
  x >>= (liftC f)

makeState a [s] : a -> (s# ->. (!a *: s#)) = !a *: s

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