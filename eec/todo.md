# Tasks for EEC

[x] desugar operators into list of calls  
[-] test how diff postfix levels interact  
[?] lazy implementation of tuple  
[-] desugar
  - `case x [: !_] of !(a, _) => a ; !x => x` to
  - `let !p = x in case p of (a, _) => a; x => x`

"They rely on giving the products a lazy interpretation: components are only
evaluated once projected out. (So, e.g., the linearity in the rule for 1
is correct because 1 is the empty product and () can never be projected.)"

(i)  Γ | −     ⊢ t: A
(ii) Γ | z: A# ⊢ t: B#

"On the right of Γ is a stoup (following the terminology of [13]),
which may either be empty, as in the case of judgement (i),
or may consist of a unique type assignment z:A,
in which case the type on the right of the turnstyle is also required to be a
computation type, as in (ii)."
  - "The purpose of judgement (i) is merely to assert that the term t has value
    type A in (value) context Γ."
  - "Judgement (ii) asserts that t is a computation of type B (in context Γ)
    which depends linearly on the computation z of type A."

* In branch `unification1`- realised i need to make scope have separate types for
  names that refer to terms and names that refer to pure types
    - e.g. when ! is used as a type `! Integer`, and also to refer to the
      constructor of ! types, e.g. `!0 : ! Integer`

* Note however that since types can currently be only defined in the `_root_` pkg
  and there is no user mechanism to define them, then it is safe to substitute
  `TypeRef(a)`, where `a` is unknown, with any concrete type - substitution must
  be propagated

[!] - DONT IMPLEMENT `::` unless you have implemented multi expr defs

- If a term `t` has computation type, and contains a parameter `z` of
  computation type then there is a natural notion of `t` depending linearly on
  `z`: the execution of the computation `t` contains within it exactly one
  execution of the computation `z`.
- such a linear dependency can only hold in general if the execution of `z` is
  the first subcomputation performed in the execution of `t`. (If, for example,
  a computation that diverges were due to be performed before `z` then `z` might
  never be executed.)
- `t` depends linearly on `z` if the execution of the computation `t` begins
  with the execution of the computation `z`.
- `t[z]` is an evaluation context -- i.e. this substitutes `z` wherever `z`
  occurs in `t`.
- Since a linearly-used parameter `z` (necessarily of computation type)
  must be executed first in the execution of `t`, it is natural that just one
  variable can enjoy this property.

for linear pattern match on tuples - keep track of linear variables,
  - e.g. if it duplicates linear arg then is it possible to duplicate by
    projection
  - I currently think not because `| (a: A#) -* | case (a,a) of p -* p`
    has not projected, so still lazy and computations not evaluated
  - in any case, each time a product is used and consumes the linear var
    then make a branch of usages. Then fold the branches to ensure that each
    one has one usage.

## Interpretation of Evaluation and linearity

I am going with `t[z]` means that `z` substituted into `t` where it exists.
  - i.e. evaluation only happens by either primitives or the language constructs.
  - A.K.A. `|(z: A#) -* ()` never evaluates `z`, but `!A -* ()`
    is able to evaluate `z` using `|(z: !A) -* let !_ = z in ()` and then give back nothing.
  - `A -* () ∼= ()` confirmed by EEC paper that its fine!!!