# To Add to Specification Report
	
* explain why match on `!a` is unsound

* explain transition from binary tuples to n-ary tuples

* explain also that at first, never had the stoup, which is required for !_
  with example that is unsound

* first choice for patt match was to try and unify after patterns are bound,
  which caused conflicts such as `Either l r != l` and having to rebind types
  - at least for checking, the codomains should be unified for constructor and
    functor type, then the subpatterns are checked on that
  - unknown yet for wildcard check

* can you match on n-ary tuples and preserve linearity

* fst and snd primitives are achieved with linear patt match, where only 1 variable may be bound

* highlight all de-sugaring or encodings of eec primitives

* Explain exhaustivity checking - not being faithful