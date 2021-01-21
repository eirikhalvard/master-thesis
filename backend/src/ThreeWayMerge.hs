module ThreeWayMerge where

import Merge.ChangeDetection (constructModificationLevelEP, flattenEvolutionPlan)
import Merge.CheckPlan (integrateAllModifications, unflattenEvolutionPlan)
import Merge.PlanMerging (createMergePlan, unifyMergePlan)
import Merge.Types
import Types

threeWayMerge ::
  UserLevelEvolutionPlan TreeFeatureModel ->
  UserLevelEvolutionPlan TreeFeatureModel ->
  UserLevelEvolutionPlan TreeFeatureModel ->
  Either Conflict (UserLevelEvolutionPlan TreeFeatureModel)
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
  Either Conflict (UserLevelEvolutionPlan TreeFeatureModel)
threeWayMerge' base v1 v2 =
  unifyMergePlan mergePlan
    >>= integrateAllModifications
    >>= unflattenEvolutionPlan
  where
    mergePlan = createMergePlan base v1 v2

conflictErrorMsg :: Conflict -> String
conflictErrorMsg conflict = "Legit error messages not implemented:)"
