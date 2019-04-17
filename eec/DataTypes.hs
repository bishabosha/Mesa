package eec.report.example

data Triple x y z =
    InX x
  | InY y
  | InZ z

data L |: R =
    Left L
  | Right R

data L# +: R# =
    InL[L#]
  | InR[R#]

data Maybe e =
    Just e
  | Nothing

{-| recursive data types are not supported -}
    
maybeZero x : Integer -> Maybe Integer =
  case x of
    0 => Just 0;
    _ => Nothing;

tripleToEither t : Triple X Y Z -> X |: Y |: Z =
  case t of
    InX x => Left x;
    InY y => Right (Left y);
    InZ z => Right (Right z);

eitherToTriple e : X |: Y |: Z -> Triple X Y Z =
  case e of
    Left x          => InX x;
    Right (Left y)  => InY y;
    Right (Right z) => InZ z;