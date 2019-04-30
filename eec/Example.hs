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

-- equiv of lin haskell
primitive read : MArray A ->. Integer -> !(MArray A, !A)

{-
linear haskell version:
foo2 :: Unrestricted Int ->. Int
-}
foo2 [mx] : !Integer ->. !Integer = let !x = mx in !(x + x)

{-
newMArray :: Int -> (MArray a ->. Unrestricted b) ->. b
-}
primitive newMArray : Integer -> (MArray a ->. !b) ->. !b

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