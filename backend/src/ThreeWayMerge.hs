{-# LANGUAGE FlexibleContexts #-}

module ThreeWayMerge where

import Convertable
import Merge.CheckPlan (integrateAndCheckModifications)
import Merge.PlanMerging (createMergePlan, unifyMergePlan)
import Types

threeWayMerge ::
  ConvertableInput inputEvolutionPlan FlatModificationEvolutionPlan =>
  MergeInputData inputEvolutionPlan ->
  MergeOutput
threeWayMerge (MergeInputData _ base v1 v2 _) = do
  let mergePlan =
        createMergePlan (convertFrom base) (convertFrom v1) (convertFrom v2)
  mergedModificationPlan <- unifyMergePlan mergePlan
  checkedUserFlatPlan <- integrateAndCheckModifications mergedModificationPlan
  return (mergedModificationPlan, checkedUserFlatPlan)
