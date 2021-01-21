module ThreeWayMerge where

import Merge.ChangeDetection (constructModificationLevelEP, flattenEvolutionPlan)
import Merge.CheckPlan (integrateAllModifications, unflattenEvolutionPlan)
import Merge.PlanMerging (createMergePlan, unifyMergePlan)
import Merge.Types
import Types

threeWayMerge ::
  AbstractedLevelEvolutionPlan FeatureModel ->
  AbstractedLevelEvolutionPlan FeatureModel ->
  AbstractedLevelEvolutionPlan FeatureModel ->
  Either Conflict (AbstractedLevelEvolutionPlan FeatureModel)
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
  ModificationLevelEvolutionPlan FeatureModel' ->
  ModificationLevelEvolutionPlan FeatureModel' ->
  ModificationLevelEvolutionPlan FeatureModel' ->
  Either Conflict (AbstractedLevelEvolutionPlan FeatureModel)
threeWayMerge' base v1 v2 =
  unifyMergePlan mergePlan
    >>= integrateAllModifications
    >>= unflattenEvolutionPlan
  where
    mergePlan = createMergePlan base v1 v2

conflictErrorMsg :: Conflict -> String
conflictErrorMsg conflict = "Legit error messages not implemented:)"
