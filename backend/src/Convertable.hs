{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Convertable where

import Merge.ChangeDetection
import Merge.CheckPlan
import ThreeWayMerge
import Types

-- defines all combinations to convert between UserTree <-> UserFlat <-> ModFlat

-- Defines a convertion between different evolution plans Since we assume the
-- evolution plans are correct, we can define variants of the algorithms in
-- CheckPlan, where we throw away the conflict, since it should not occur.
-- Therefor we define "unsafe" variants that just works on sound evolution
-- plans

class ConvertableInput input output where
  convertFrom :: input -> output

integrateAllSoundModifications :: FlatModificationEvolutionPlan -> FlatUserEvolutionPlan
integrateAllSoundModifications =
  either (error . conflictErrorMsg) id . integrateAllModifications

unflattenSoundEvolutionPlan :: FlatUserEvolutionPlan -> TreeUserEvolutionPlan
unflattenSoundEvolutionPlan =
  either (error . conflictErrorMsg) id . unflattenEvolutionPlan

instance ConvertableInput TreeUserEvolutionPlan TreeUserEvolutionPlan where
  convertFrom = id

instance ConvertableInput TreeUserEvolutionPlan FlatUserEvolutionPlan where
  convertFrom = flattenEvolutionPlan

instance ConvertableInput TreeUserEvolutionPlan FlatModificationEvolutionPlan where
  convertFrom = constructModificationEP . flattenEvolutionPlan

instance ConvertableInput FlatUserEvolutionPlan TreeUserEvolutionPlan where
  convertFrom = unflattenSoundEvolutionPlan

instance ConvertableInput FlatUserEvolutionPlan FlatUserEvolutionPlan where
  convertFrom = id

instance ConvertableInput FlatUserEvolutionPlan FlatModificationEvolutionPlan where
  convertFrom = constructModificationEP

instance ConvertableInput FlatModificationEvolutionPlan TreeUserEvolutionPlan where
  convertFrom = unflattenSoundEvolutionPlan . integrateAllSoundModifications

instance ConvertableInput FlatModificationEvolutionPlan FlatUserEvolutionPlan where
  convertFrom = integrateAllSoundModifications

instance ConvertableInput FlatModificationEvolutionPlan FlatModificationEvolutionPlan where
  convertFrom = id

-- Defines a way of converting the result of the merge into the specified
-- output format. Since the merger both produces the
-- FlatModificationEvolutionPlan and AbsFlat evolution plans, we can utilize
-- both in the convertion

class ConvertableFromResult output where
  convertFromMergeResult :: FlatModificationEvolutionPlan -> FlatUserEvolutionPlan -> output

instance ConvertableFromResult TreeUserEvolutionPlan where
  convertFromMergeResult _ userFlat = unflattenSoundEvolutionPlan userFlat

instance ConvertableFromResult FlatUserEvolutionPlan where
  convertFromMergeResult _ userFlat = userFlat

instance ConvertableFromResult FlatModificationEvolutionPlan where
  convertFromMergeResult modFlat _ = modFlat
