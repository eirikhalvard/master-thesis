module Merge.Types where

import Types

data Conflict
  = Merge Time MergeConflict
  | Local Time LocalConflict
  | Global Time GlobalConflict
  | Panic Time String
  deriving (Show, Eq)

data MergeConflict
  = FeatureConflict (BothChange FeatureModification)
  | GroupConflict (BothChange GroupModification)
  deriving (Show, Eq)

data LocalConflict
  = ConflictingModifications
  | OthersEtcEtcEtc
  deriving (Show, Eq)

data GlobalConflict
  = DuplicateName
  | EtcEtcEtc
  deriving (Show, Eq)
