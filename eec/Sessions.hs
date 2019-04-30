package eec.report.example

-- concat *:

-- let !_ *: rchan1      = newChan in
-- let !_                = (send server)[!"open foo" *: rchan1] in
-- let !_ *: schan2      = recieve[rchan1] in
-- let !ch *: schan2     = !mkChar *: schan2 in
-- let !ch *: (rchan2,schan2) = !ch *: (newChan in
-- (send schan2)[!ch *: rchan2]