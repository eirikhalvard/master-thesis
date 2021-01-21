module ThreeWayMerge where

import Merge.ChangeDetection (constructModificationLevelEP, flattenEvolutionPlan)
import Merge.CheckPlan (integrateAllModifications, unflattenEvolutionPlan)
import Merge.PlanMerging (createMergePlan, unifyMergePlan)
import Merge.Types
import Types

threeWayMerge ::
  AbstractedLevelEvolutionPlan TreeFeatureModel ->
  AbstractedLevelEvolutionPlan TreeFeatureModel ->
  AbstractedLevelEvolutionPlan TreeFeatureModel ->
  Either Conflict (AbstractedLevelEvolutionPlan TreeFeatureModel)
threeWayMerge base v1 v2 =
  unifyMergePlan mergePlan
    >>= integrateAllModifications
    >>= unflattenEvolutionPlan
  where
    mergePlan =
      createMergePlan
        (constructModificationLevelEP . flattenEvolutionPlan $ base)
        (constructModificationLevelEP . flattenEvolutionPlan $ v1)
        (constructModificationLevelEP . flattenEvolutionPlan $ v2)

threeWayMerge' ::
  ModificationLevelEvolutionPlan FlatFeatureModel ->
  ModificationLevelEvolutionPlan FlatFeatureModel ->
  ModificationLevelEvolutionPlan FlatFeatureModel ->
  Either Conflict (AbstractedLevelEvolutionPlan TreeFeatureModel)
threeWayMerge' base v1 v2 =
  unifyMergePlan mergePlan
    >>= integrateAllModifications
    >>= unflattenEvolutionPlan
  where
    mergePlan = createMergePlan base v1 v2

conflictErrorMsg :: Conflict -> String
conflictErrorMsg conflict = "Legit error messages not implemented:)"
