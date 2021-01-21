module ThreeWayMerge where

import Merge.ChangeDetection (constructModificationEP, flattenEvolutionPlan)
import Merge.CheckPlan (integrateAllModifications, unflattenEvolutionPlan)
import Merge.PlanMerging (createMergePlan, unifyMergePlan)
import Types

threeWayMerge ::
  TreeUserEvolutionPlan ->
  TreeUserEvolutionPlan ->
  TreeUserEvolutionPlan ->
  Either Conflict TreeUserEvolutionPlan
threeWayMerge base v1 v2 =
  unifyMergePlan mergePlan
    >>= integrateAllModifications
    >>= unflattenEvolutionPlan
  where
    mergePlan =
      createMergePlan
        (constructModificationEP . flattenEvolutionPlan $ base)
        (constructModificationEP . flattenEvolutionPlan $ v1)
        (constructModificationEP . flattenEvolutionPlan $ v2)

threeWayMerge' ::
  FlatModificationEvolutionPlan ->
  FlatModificationEvolutionPlan ->
  FlatModificationEvolutionPlan ->
  Either Conflict TreeUserEvolutionPlan
threeWayMerge' base v1 v2 =
  unifyMergePlan mergePlan
    >>= integrateAllModifications
    >>= unflattenEvolutionPlan
  where
    mergePlan = createMergePlan base v1 v2

conflictErrorMsg :: Conflict -> String
conflictErrorMsg conflict = "Legit error messages not implemented:)"
