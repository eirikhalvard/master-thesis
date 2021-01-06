module Merge.CheckPlan where

import Merge.Types
import Types

import qualified Data.Map as M
import qualified Data.Set as S

------------------------------------------------------------------------
--                    Integrate All Modifications                     --
------------------------------------------------------------------------

integrateAllModifications ::
  ModificationLevelEvolutionPlan FeatureModel' ->
  Either Conflict (AbstractedLevelEvolutionPlan FeatureModel')
integrateAllModifications mergedPlan = undefined

------------------------------------------------------------------------
--                      Unflatten Evolution Plan                      --
------------------------------------------------------------------------

unflattenEvolutionPlan ::
  AbstractedLevelEvolutionPlan FeatureModel' ->
  Either Conflict (AbstractedLevelEvolutionPlan FeatureModel)
unflattenEvolutionPlan flatEvolutionPlan = undefined
