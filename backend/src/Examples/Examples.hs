module Examples.Examples where

import qualified Data.Map as M

import Examples.GlobalConflictExample
import Examples.LocalConflictExample
import Examples.MergeConflictExample
import Examples.SoundExample
import Types

mergeData :: M.Map String MergeInput
mergeData =
  M.fromList
    [ ("SoundExample", TreeUser soundExample)
    , ("ConflictMultipleAdd", FlatModification multipleAdd)
    , ("ConflictRemoveAndChange", FlatModification removeAndChangeModification)
    , ("MovedAddition", FlatModification movedFeatureAddition)
    , ("ConflictingAdditionMove", FlatModification conflictingAdditionMove)
    , ("ConflictingGroupRemove", FlatModification conflictingGroupRemove)
    , ("MoveGroupCycle", FlatModification groupMoveCycle)
    , ("WellFormedViolation", FlatModification violatingFeatureWellFormed)
    , ("MissingParentFeature", FlatModification missingParentFeature)
    ]
