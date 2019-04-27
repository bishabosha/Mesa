package eec.report.example

-- Please import the Prelude by using the `-p` flag

data Either3 x y z      = In1 x | In2 y | In3 z
data Either3C a# b# c#  = In1C[a#] | In2C[b#] | In3C[c#]
data MaybeC a#          = JustC[a#] | NothingC
data Maybe a            = Just a | Nothing
data Maybe3 a b c       = Just3 a b c | Nothing3

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

boolean_to_or b: Boolean -> () |: () =
  if b then Left () else Right ()

or_to_boolean b: () |: () -> Boolean =
  case b of
    Left _ => True;
    _      => False;

either3_to_or t : Either3 X Y Z -> X |: Y |: Z =
  case t of
    In1 x => Left x;
    In2 y => Right (Left y);
    In3 z => Right (Right z);

or_to_either3 e : X |: Y |: Z -> Either3 X Y Z =
  case e of
    Left x          => In1 x;
    Right (Left y)  => In2 y;
    Right (Right z) => In3 z;

either3c_to_orC [t] : Either3C X# Y# Z# ->. X# +: Y# +: Z# =
  case t of
    In1C[x] =>. InL[x];
    In2C[y] =>. InR[InL[y]];
    In3C[z] =>. InR[InR[z]];

orC_to_either3c [e] : X# +: Y# +: Z# ->. Either3C X# Y# Z# =
  case e of
    InL[x]      =>. In1C[x];
    InR[InL[y]] =>. In2C[y];
    InR[InR[z]] =>. In3C[z];

maybec_to_orC [m]: MaybeC A# ->. A# +: () =
  case m of
    JustC[a] =>. InL[a];
    _        =>. InR[()];

orC_to_maybec [s]: A# +: () ->. MaybeC A# =
  case s of
    InL[a] =>. JustC[a];
    _      =>. NothingC;

orV_to_a e: Void |: A -> A =
  case e of
    Left v  => ?v;
    Right a => a;

a_to_orV a: A -> Void |: A =
  Right a

orCV_to_a [e]: Void# +: A# ->. A# =
  case e of
    InL[v] =>. absurd[v];
    InR[a] =>. a;

a_to_orCV [a]: A# ->. Void# +: A# =
  InR[a]