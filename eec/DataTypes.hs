package eec.report.example

data Either3 x y z =
    In1 x
  | In2 y
  | In3 z

data L |: R =
    Left L
  | Right R

data L# +: R# =
    InL[L#]
  | InR[R#]

data MaybeC A# =
    JustC[A#]
  | NothingC

data Maybe a =
    Just a
  | Nothing

data Maybe3 a b c =
    Just3 a b c
  | Nothing3

{-| recursive data types are not supported -}

maybe_to_or m : Maybe A -> A |: () =
  case m of
    Just a  => Left  a;
    _       => Right ();

or_to_maybe m : A |: () -> Maybe A =
  case m of
    Left a  => Just a;
    _       => Nothing;
    
maybe_to_maybe3 m : Maybe (A, B, C) -> Maybe3 A B C =
  case m of
    Just (a, b, c) => Just3 a b c;
    _              => Nothing3;

maybe3_to_maybe m : Maybe3 A B C -> Maybe (A, B, C) =
  case m of
    Just3 a b c => Just (a, b, c);
    _           => Nothing;

maybe3_to_or3 m : Maybe3 A B C -> (A, B, C) |: () =
  case m of
    Just3 a b c => Left (a, b, c);
    _           => Right ();

or3_to_maybe3 m : (A, B, C) |: () -> Maybe3 A B C =
  case m of
    Left (a, b, c) => Just3 a b c;
    _              => Nothing3;

maybe_to_or3 m : Maybe (A, B, C) -> (A, B, C) |: () =
  case m of
    Just (a, b, c) => Left (a, b, c);
    _              => Right ();

or3_to_maybe m : (A, B, C) |: () -> Maybe (A, B, C) =
  case m of
    Left (a, b, c) => Just (a, b, c);
    _              => Nothing;

boolean_to_or b: Boolean -> () |: () =
  case b of
    True  => Left  ();
    _     => Right ();

or_to_boolean b: () |: () -> Boolean =
  case b of
    Left () => True;
    _       => False;

either3_to_stacked t : Either3 X Y Z -> X |: Y |: Z =
  case t of
    In1 x => Left x;
    In2 y => Right (Left y);
    In3 z => Right (Right z);

stacked_to_either3 e : X |: Y |: Z -> Either3 X Y Z =
  case e of
    Left x          => In1 x;
    Right (Left y)  => In2 y;
    Right (Right z) => In3 z;

maybec_to_compsum m: MaybeC A# -> A# +: () =
  case m of
    JustC[a] -○ InL[a];
    _        -○ InR[()];

compsum_to_maybec s: A# +: () -> MaybeC A# =
  case s of
    InL[a] -○ JustC[a];
    _      -○ NothingC;