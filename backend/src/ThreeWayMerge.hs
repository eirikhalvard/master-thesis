module ThreeWayMerge where

import Merge.ChangeDetection (deriveSoundModifications, flattenSoundEvolutionPlan)
import Merge.CheckPlan (integrateAndCheckModifications, unflattenSoundEvolutionPlan)
import Merge.PlanMerging (createMergePlan, unifyMergePlan)
import Types

threeWayMerge ::
  TreeUserEvolutionPlan ->
  TreeUserEvolutionPlan ->
  TreeUserEvolutionPlan ->
  Either Conflict TreeUserEvolutionPlan
threeWayMerge base v1 v2 = do
  let mergePlan =
        createMergePlan
          (deriveSoundModifications . flattenSoundEvolutionPlan $ base)
          (deriveSoundModifications . flattenSoundEvolutionPlan $ v1)
          (deriveSoundModifications . flattenSoundEvolutionPlan $ v2)
  mergedModificationPlan <- unifyMergePlan mergePlan
  checkedUserFlatPlan <- integrateAndCheckModifications mergedModificationPlan
  return $ unflattenSoundEvolutionPlan checkedUserFlatPlan

threeWayMerge' ::
  FlatModificationEvolutionPlan ->
  FlatModificationEvolutionPlan ->
  FlatModificationEvolutionPlan ->
  Either Conflict TreeUserEvolutionPlan
threeWayMerge' base v1 v2 = do
  let mergePlan = createMergePlan base v1 v2
  mergedModificationPlan <- unifyMergePlan mergePlan
  checkedUserFlatPlan <- integrateAndCheckModifications mergedModificationPlan
  return $ unflattenSoundEvolutionPlan checkedUserFlatPlan

conflictErrorMsg :: Conflict -> String
conflictErrorMsg conflict =
  "Legit error messages not implemented:) Using the Show instance of the conflict:\n"
    ++ show conflict
