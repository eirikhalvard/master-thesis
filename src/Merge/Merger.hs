module Merge.Merger where

import           Merge.MergeTypes
import           Types

import qualified Data.Map         as M
import qualified Data.Set         as S


data MergeConflict
  = LocalConflict LocalConflict
  | GlobalConflict GlobalConflict


data LocalConflict
  = ConflictingModifications
  | OthersEtcEtcEtc


data GlobalConflict
  = DuplicateName
  | EtcEtcEtc


diffModifications
  :: ModificationLevelEvolutionPlan
  -> ModificationLevelEvolutionPlan
  -> ModificationLevelEvolutionPlan
  -> MergeLevelEvolutionPlan
diffModifications base v1 v2 = undefined


mergeAllChanges
  :: MergeLevelEvolutionPlan
  -> Either MergeConflict ModificationLevelEvolutionPlan
mergeAllChanges mergeLevelEP = undefined


deriveAbstractedEvolutionPlan
  :: ModificationLevelEvolutionPlan
  -> AbstractedLevelEvolutionPlan
deriveAbstractedEvolutionPlan modificationLevelEP = undefined
