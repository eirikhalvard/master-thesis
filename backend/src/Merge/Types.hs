module Merge.Types where

import Types

data Conflict
  = Merge Time MergeConflict
  | Local Time LocalConflict
  | Global Time GlobalConflict
  | Panic Time String

data MergeConflict
  = ConflictingFeatureModificationWithoutBase
      FeatureModification
      FeatureModification
  | ConflictingGroupModificationWithoutBase
      GroupModification
      GroupModification
  | ConflictingFeatureModificationWithBase
      FeatureModification
      FeatureModification
      FeatureModification
  | ConflictingGroupModificationWithBase
      GroupModification
      GroupModification
      GroupModification

data LocalConflict
  = ConflictingModifications
  | OthersEtcEtcEtc

data GlobalConflict
  = DuplicateName
  | EtcEtcEtc
