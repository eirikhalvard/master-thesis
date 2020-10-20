module Merge.Merger where

import           Merge.MergeTypes
import           Types

import qualified Data.Map         as M
import qualified Data.Set         as S


data MergeConflict
  = DuplicateName
  | EtcEtcEtc


threeWayMerge
  :: MergeLevelEvolutionPlan
  -> MergeLevelEvolutionPlan
  -> MergeLevelEvolutionPlan
  -> Either MergeConflict MergeLevelEvolutionPlan
threeWayMerge base v1 v2 = undefined



