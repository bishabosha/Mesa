package eec.report.example

primitive absurd v    : Void -> A
primitive Left  l     : L -> Either L R
primitive Right r     : R -> Either L R
primitive summon [v]  : Void# |- A#
primitive InL [l]     : L# |- L# +: R#
primitive InR [r]     : R# |- L# +: R#

fstL [p]: (A#, B#) |- A# =
  case [p] of (a, _) |- a

sndL [p]: (A#, B#) |- B# =
  case [p] of (_, b) |- b

fst p: (A, B) -> A =
  case p of (a, _) => a

snd p: (A, B) -> B =
  case p of (_, b) => b

isomorphism_2a f [t]: (A -> B#) -> (!A |- B#) =
  let !x = t in f x

isomorphism_2b f a: (!A |- B#) -> A -> B# =
  f [!a]

isomorphism_3_1a f a [b]: (!A *: B# |- C#) -> A -> (B# |- C#) =
  f [!a *: b]

isomorphism_3_1b f [t]: (A -> (B# |- C#)) -> (!A *: B# |- C#) =
  let !a *: b = t in
  f a [b]

isomorphism_3_2a f [b]: (A -> (B# |- C#)) -> (B# |- (A -> C#)) =
  \(a: A) => f a [b]

isomorphism_3_2b f a: (B# |- (A -> C#)) -> A -> (B# |- C#) =
  |(b: B#) |- f [b] a

isomorphism_3_3a f [b]: (!A *: B# |- C#) -> (B# |- (A -> C#)) =
  \(a: A) => f [!a *: b]

isomorphism_3_3b f [t]: (B# |- (A -> C#)) -> (!A *: B# |- C#) =
  let !a *: b = t in
  f [b] a

isomorphism_4a _ : (A# |- ()) -> () =
  ()

isomorphism_4b _ : () -> (A# |- ()) =
  |(a: A#) |- ()

isomorphism_5a f : (A# |- (B#, C#)) -> (A# |- B#, A# |- C#) =
  (|(a: A#) |- fstL [f [a]], | (a: A#) |- sndL [f [a]])

isomorphism_5b p [a] : (A# |- B#, A# |- C#) -> (A# |- (B#, C#)) =
  (fst p [a], snd p [a])

isomorphism_6a _ : (Void# |- A#) -> () =
  ()

isomorphism_6b _ : () -> (Void# |- A#) =
  summon

isomorphism_7a f : (A# +: B# |- C#) -> (A# |- C#, B# |- C#) =
  (| (a: A#) |- f [InL [a]], | (b: B#) |- f [InR [b]])

isomorphism_7b p [e] : (A# |- C#, B# |- C#) -> (A# +: B# |- C#) =
  case [e] of
    InL [a] |- fst p [a]
    InR [b] |- snd p [b]

isomorphism_8a [w]: !A *: !B |- !(A, B) =
  let !x *: z = w in
  let !y      = z in
  !(x,y)

isomorphism_8b [w] : !(A, B) |- !A *: !B =
  let !z = w in
  !(fst z) *: !(snd z)

isomorphism_9a [t] : !A *: Void# |- Void# =
  let !_ *: v = t in v

isomorphism_9b : Void# |- !A *: Void# =
  summon

isomorphism_10a [t] : !A *: (B# +: C#) |- (!A *: B#) +: (!A *: C#) =
  let !a *: e = t in
  case [e] of
    InL [b] |- InL [!a *: b]
    InR [c] |- InR [!a *: c]

isomorphism_10b [e] : (!A *: B#) +: (!A *: C#) |- !A *: (B# +: C#) =
  case [e] of
    InL [t] |-
      let !a *: b = t in
      !a *: (InL [b])
    InR [u] |-
      let !a *: c = u in
      !a *: (InR [c])

isomorphism_11a [t] : !() *: A# |- A# =
  let !_ *: a = t in a

isomorphism_11b [a] : A# |- !() *: A# =
  !() *: a

isomorphism_12a [v]: !(A, B) *: C# |- !A *: !B *: C# =
  let !p *: z = v in
  !(fst p) *: !(snd p) *: z

isomorphism_12b [v] : !A *: !B *: C# |- !(A, B) *: C# =
  let !x *: w = v in
  let !y *: z = w in
  !(x,y) *: z

isomorphism_13a [t] : !Void |- Void# =
  let !v = t in
  absurd v

isomorphism_13b : Void# |- !Void =
  summon

isomorphism_14a [t] : !(Either A B) |- !A +: !B =
  let !e = t in
  case e of
    Left a  => InL [!a];
    Right b => InR [!b];

isomorphism_14b [e] : !A +: !B |- !(Either A B) =
  case [e] of
    InL [t] |-
      let !a = t in
      !(Left a)
    InR [u] |-
      let !b = u in
      !(Right b)

isomorphism_15a [t]: !Void *: A# |- Void# =
  let !v *: a = t in
  absurd v

isomorphism_15b: Void# |- !Void *: A# =
  summon

isomorphism_16a [t] : !(Either A B) *: C# |- (!A *: C#) +: (!B *: C#) =
  let !e *: c = t in
  case e of
    Left a  => InL [!a *: c];
    Right b => InR [!b *: c];

isomorphism_16b [e] : (!A *: C#) +: (!B *: C#) |- !(Either A B) *: C# =
  case [e] of
    InL [t] |-
      let !a *: c = t in
      !(Left a) *: c
    InR [u] |-
      let !b *: c = u in
      !(Right b) *: c