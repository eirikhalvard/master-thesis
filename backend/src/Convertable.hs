{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Convertable where

import Merge.ChangeDetection
import Merge.CheckPlan
import ThreeWayMerge
import Types

-- defines all combinations to convert between UserTree <-> UserFlat <-> ModFlat

-- Defines a convertion between different evolution plans
-- Since we assume the evolution plans are correct, we can define
-- variants of the algorithms in CheckPlan, where we throw away
-- the conflict, since it should not occur. Therefor we define "unsafe"
-- variants that just works on sound evolution plans

class ConvertableInput input output where
  convertFrom :: input -> output

integrateAllSoundModifications :: ModificationEvolutionPlan FlatFeatureModel -> UserEvolutionPlan FlatFeatureModel
integrateAllSoundModifications =
  either (error . conflictErrorMsg) id
    . integrateAllModifications

unflattenSoundEvolutionPlan :: UserEvolutionPlan FlatFeatureModel -> UserEvolutionPlan TreeFeatureModel
unflattenSoundEvolutionPlan =
  either (error . conflictErrorMsg) id
    . unflattenEvolutionPlan

instance ConvertableInput (UserEvolutionPlan TreeFeatureModel) (UserEvolutionPlan TreeFeatureModel) where
  convertFrom = id

instance ConvertableInput (UserEvolutionPlan TreeFeatureModel) (UserEvolutionPlan FlatFeatureModel) where
  convertFrom = flattenEvolutionPlan

instance ConvertableInput (UserEvolutionPlan TreeFeatureModel) (ModificationEvolutionPlan FlatFeatureModel) where
  convertFrom = constructModificationEP . flattenEvolutionPlan

instance ConvertableInput (UserEvolutionPlan FlatFeatureModel) (UserEvolutionPlan TreeFeatureModel) where
  convertFrom = unflattenSoundEvolutionPlan

instance ConvertableInput (UserEvolutionPlan FlatFeatureModel) (UserEvolutionPlan FlatFeatureModel) where
  convertFrom = id

instance ConvertableInput (UserEvolutionPlan FlatFeatureModel) (ModificationEvolutionPlan FlatFeatureModel) where
  convertFrom = constructModificationEP

instance ConvertableInput (ModificationEvolutionPlan FlatFeatureModel) (UserEvolutionPlan TreeFeatureModel) where
  convertFrom = unflattenSoundEvolutionPlan . integrateAllSoundModifications

instance ConvertableInput (ModificationEvolutionPlan FlatFeatureModel) (UserEvolutionPlan FlatFeatureModel) where
  convertFrom = integrateAllSoundModifications

instance ConvertableInput (ModificationEvolutionPlan FlatFeatureModel) (ModificationEvolutionPlan FlatFeatureModel) where
  convertFrom = id

-- Defines a way of converting the result of the merge into the specified
-- output format. Since the merger both produces the (ModificationEvolutionPlan FlatFeatureModel) and AbsFlat
-- evolution plans, we can utilize both in the convertion

class ConvertableFromResult output where
  convertFromMergeResult ::
    (ModificationEvolutionPlan FlatFeatureModel) ->
    (UserEvolutionPlan FlatFeatureModel) ->
    output

instance ConvertableFromResult (UserEvolutionPlan TreeFeatureModel) where
  convertFromMergeResult _ userFlat = unflattenSoundEvolutionPlan userFlat

instance ConvertableFromResult (UserEvolutionPlan FlatFeatureModel) where
  convertFromMergeResult _ userFlat = userFlat

instance ConvertableFromResult (ModificationEvolutionPlan FlatFeatureModel) where
  convertFromMergeResult modFlat _ = modFlat
