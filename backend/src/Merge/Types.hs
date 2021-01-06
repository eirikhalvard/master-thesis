module Merge.Types where

import Types

data Conflict
  = Merge Time MergeConflict
  | Local Time LocalConflict
  | Global Time GlobalConflict
  | Panic Time String

data MergeConflict
  = FeatureConflict (BothChange FeatureModification)
  | GroupConflict (BothChange GroupModification)

data LocalConflict
  = ConflictingModifications
  | OthersEtcEtcEtc

data GlobalConflict
  = DuplicateName
  | EtcEtcEtc
