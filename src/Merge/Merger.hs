module Merge.Merger where

import           Types

import qualified Data.Map as M
import qualified Data.Set as S


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
  :: ModificationLevelEvolutionPlan FeatureModel
  -> ModificationLevelEvolutionPlan FeatureModel
  -> ModificationLevelEvolutionPlan FeatureModel
  -> MergeLevelEvolutionPlan FeatureModel
diffModifications base v1 v2 = undefined


mergeAllChanges
  :: MergeLevelEvolutionPlan FeatureModel
  -> Either MergeConflict (ModificationLevelEvolutionPlan FeatureModel)
mergeAllChanges mergeLevelEP = undefined


deriveAbstractedEvolutionPlan
  :: ModificationLevelEvolutionPlan FeatureModel
  -> AbstractedLevelEvolutionPlan FeatureModel
deriveAbstractedEvolutionPlan modificationLevelEP = undefined
