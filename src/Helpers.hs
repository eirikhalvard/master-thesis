module Helpers (notExists, parentOfGroup) where

import           Types
import           Example1

import           Control.Lens
import qualified Data.Set                      as S
import qualified Data.Map                      as M

notExists :: FeatureID -> FeatureTable -> Bool
notExists fid = hasn't (ix fid)

parentOfGroup :: GroupID -> FeatureTable -> Maybe FeatureID
parentOfGroup gid =
  preview (itraversed . filtered (has (groups . ix gid)) . asIndex)

main :: IO ()
main = do
  let printDivider s = putStr "\n\n\n--- " >> putStr s >> putStrLn "\n"
      carFeatureTable = view features carExample
  print "hei"
  print $ carFeatureTable & notExists 2
  print $ carFeatureTable & notExists 4
  print $ carFeatureTable & parentOfGroup 21
  print $ carFeatureTable & parentOfGroup 22
  print $ carFeatureTable & parentOfGroup 11

