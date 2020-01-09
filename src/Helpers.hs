{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
module Helpers where

import           Types
import           Example1

import           Control.Lens
import qualified Data.Set                      as S
import qualified Data.Map                      as M

notExists :: FeatureID -> FeatureModel -> Bool
notExists fid = hasn't (features . ix fid)

main :: IO ()
main = do
  let printDivider s = putStr "\n\n\n--- " >> putStr s >> putStrLn "\n"
  print "hei"

