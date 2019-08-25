package mesa.report.example

-- Please import the Prelude by using the `-p` flag

primitive (+) : Integer -> Integer -> Integer
primitive (++) : String -> String -> String
primitive debug : A -> String
primitive putStrLn : String -> !()
primitive readInteger : !Integer

liftC f x : (A -> B) -> A -> !B =
  !(f x)

x >>= f : !A -> (A -> !B) -> !B =
  let !y = x in f y

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