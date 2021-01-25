module Conflict where

import Types

conflictErrorMsg :: Conflict -> String
conflictErrorMsg conflict =
  "Legit error messages not implemented:) Using the Show instance of the conflict:\n"
    ++ show conflict
