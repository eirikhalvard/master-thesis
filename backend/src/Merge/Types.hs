module Merge.Types where

data Conflict
  = Merge MergeConflict
  | Local LocalConflict
  | Global GlobalConflict
  | Panic String

data MergeConflict
  = MergeConflict String

data LocalConflict
  = ConflictingModifications
  | OthersEtcEtcEtc

data GlobalConflict
  = DuplicateName
  | EtcEtcEtc
