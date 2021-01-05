module Merge.ThreeWayMerge where

import Merge.ChangeDetection (deriveChanges)
import Merge.CheckPlan (integrateAllModifications, unflattenEvolutionPlan)
import Merge.PlanMerging (createMergePlan, unifyMergePlan)
import Merge.Types
import Types

threeWayMerge ::
  AbstractedLevelEvolutionPlan FeatureModel ->
  AbstractedLevelEvolutionPlan FeatureModel ->
  AbstractedLevelEvolutionPlan FeatureModel ->
  Either Conflict (AbstractedLevelEvolutionPlan FeatureModel)
threeWayMerge base v1 v2 = unifyMergePlan mergePlan >>= integrateAllModifications >>= unflattenEvolutionPlan
  where
    mergePlan =
      createMergePlan
        (deriveChanges base)
        (deriveChanges v1)
        (deriveChanges v2)
