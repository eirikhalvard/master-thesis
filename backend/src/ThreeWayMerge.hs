{-# LANGUAGE FlexibleContexts #-}

module ThreeWayMerge where

import Convertable
import Merge.CheckPlan (integrateAndCheckModifications)
import Merge.PlanMerging (createMergePlan, unifyMergePlan)
import Types

threeWayMerge ::
  ConvertableInput inputEvolutionPlan =>
  MergeInputData inputEvolutionPlan ->
  MergeOutput
threeWayMerge (MergeInputData _ base v1 v2 _) = do
  let mergePlan =
        createMergePlan
          (toFlatModification base)
          (toFlatModification v1)
          (toFlatModification v2)
  mergedModificationPlan <- unifyMergePlan mergePlan
  checkedUserFlatPlan <- integrateAndCheckModifications mergedModificationPlan
  return (mergedModificationPlan, checkedUserFlatPlan)
