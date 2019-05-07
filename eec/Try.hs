package eec.report.example

-- Exception modelling
data Error = Error [!String]
data Try a# = Success[a#] | Fail[Error]

f =<< [t] : (a# ->. Try b#) -> Try a# ->. Try b# =
  case t of
    Fail[e]    =>. Fail[e]
    Success[a] =>. f[a]