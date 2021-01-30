module Conflict where

import Data.Text.Lazy
import Text.Pretty.Simple

import Types

conflictErrorMsg :: Conflict -> String
conflictErrorMsg conflict =
  "Legit error messages not implemented:) Using the Show instance of the conflict:\n"
    ++ unpack (pShowNoColor conflict)
