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

integrateAllSoundModifications :: ModFlat -> UserFlat
integrateAllSoundModifications =
  either (error . conflictErrorMsg) id
    . integrateAllModifications

unflattenSoundEvolutionPlan :: UserFlat -> UserTree
unflattenSoundEvolutionPlan =
  either (error . conflictErrorMsg) id
    . unflattenEvolutionPlan

instance ConvertableInput UserTree UserTree where
  convertFrom = id

instance ConvertableInput UserTree UserFlat where
  convertFrom = flattenEvolutionPlan

instance ConvertableInput UserTree ModFlat where
  convertFrom = constructModificationEP . flattenEvolutionPlan

instance ConvertableInput UserFlat UserTree where
  convertFrom = unflattenSoundEvolutionPlan

instance ConvertableInput UserFlat UserFlat where
  convertFrom = id

instance ConvertableInput UserFlat ModFlat where
  convertFrom = constructModificationEP

instance ConvertableInput ModFlat UserTree where
  convertFrom = unflattenSoundEvolutionPlan . integrateAllSoundModifications

instance ConvertableInput ModFlat UserFlat where
  convertFrom = integrateAllSoundModifications

instance ConvertableInput ModFlat ModFlat where
  convertFrom = id

-- Defines a way of converting the result of the merge into the specified
-- output format. Since the merger both produces the ModFlat and AbsFlat
-- evolution plans, we can utilize both in the convertion

class ConvertableFromResult output where
  convertFromMergeResult :: ModFlat -> UserFlat -> output

instance ConvertableFromResult UserTree where
  convertFromMergeResult _ userFlat = unflattenSoundEvolutionPlan userFlat

instance ConvertableFromResult UserFlat where
  convertFromMergeResult _ userFlat = userFlat

instance ConvertableFromResult ModFlat where
  convertFromMergeResult modFlat _ = modFlat
