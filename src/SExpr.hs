module SExpr where

import Pretty3

data SE
  = SL [SE]
  | ST String

