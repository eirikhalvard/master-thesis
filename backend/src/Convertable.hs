{-# LANGUAGE FlexibleInstances #-}

module Convertable where

import Conflict
import Merge.ChangeDetection
import Merge.CheckPlan
import Types

-- defines all combinations to convert between UserTree <-> UserFlat <-> ModFlat

-- Defines a convertion between different evolution plans Since we assume the
-- evolution plans are correct, we can define variants of the algorithms in
-- CheckPlan, where we throw away the conflict, since it should not occur.
-- Therefor we define "unsafe" variants that just works on sound evolution
-- plans

class ConvertableInput input where
  toTreeUser :: input -> TreeUserEvolutionPlan
  toFlatUser :: input -> FlatUserEvolutionPlan
  toFlatModification :: input -> FlatModificationEvolutionPlan

integrateSoundModifications :: FlatModificationEvolutionPlan -> FlatUserEvolutionPlan
integrateSoundModifications =
  either (error . conflictErrorMsg) id . integrateAndCheckModifications

instance ConvertableInput TreeUserEvolutionPlan where
  toTreeUser = id
  toFlatUser = flattenSoundEvolutionPlan
  toFlatModification = deriveSoundModifications . flattenSoundEvolutionPlan

instance ConvertableInput FlatUserEvolutionPlan where
  toTreeUser = unflattenSoundEvolutionPlan
  toFlatUser = id
  toFlatModification = deriveSoundModifications

instance ConvertableInput FlatModificationEvolutionPlan where
  toTreeUser = unflattenSoundEvolutionPlan . integrateSoundModifications
  toFlatUser = integrateSoundModifications
  toFlatModification = id

-- Defines a way of converting the result of the merge into the specified
-- output format. Since the merger both produces the
-- FlatModificationEvolutionPlan and AbsFlat evolution plans, we can utilize
-- both in the convertion

class ConvertableOutput output where
  fromMergeOutput :: FlatModificationEvolutionPlan -> FlatUserEvolutionPlan -> output

instance ConvertableOutput TreeUserEvolutionPlan where
  fromMergeOutput _ userFlat = unflattenSoundEvolutionPlan userFlat

instance ConvertableOutput FlatUserEvolutionPlan where
  fromMergeOutput _ userFlat = userFlat

instance ConvertableOutput FlatModificationEvolutionPlan where
  fromMergeOutput modFlat _ = modFlat
