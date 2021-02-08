module Conflict where

import Data.Text.Lazy
import Text.Pretty.Simple

import Types

conflictErrorMsg :: Conflict -> String
conflictErrorMsg conflict =
  case conflict of
    Merge time mergeConflict ->
      constructErrorMsg time (toErrorMessage mergeConflict)
    Local time localConflict ->
      constructErrorMsg time (toErrorMessage localConflict)
    Global time globalConflict ->
      constructErrorMsg time (toErrorMessage globalConflict)
    Panic time str ->
      constructErrorMsg time $ "The algorithm paniced! with message:\n" ++ str

class ConflictShow a where
  toErrorMessage :: a -> String

constructErrorMsg :: Int -> String -> String
constructErrorMsg time reason =
  "A conflict occured at time\n"
    ++ show time
    ++ "\n---\n"
    ++ "The conflict occured because of "
    ++ reason

instance ConflictShow MergeConflict where
  toErrorMessage (FeatureConflict bothChange) =
    toErrorMessage bothChange
  toErrorMessage (GroupConflict bothChange) =
    toErrorMessage bothChange

instance ConflictShow modificationType => ConflictShow (BothChange modificationType) where
  toErrorMessage bothChange =
    case bothChange of
      BothChangeWithBase
        baseModification
        removedOrChangedV1
        removedOrChangedV2 ->
          "conflicting operations\n"
            ++ "The base version originally had the following operation:\n"
            ++ toErrorMessage baseModification
            ++ "And version 1 tried to \n"
            ++ removeChangeHelper removedOrChangedV1
            ++ "While version 2 tried to \n"
            ++ removeChangeHelper removedOrChangedV2
      BothChangeWithoutBase
        (AddedModification v1Modification)
        (AddedModification v2Modification) ->
          "conflicting operations\n"
            ++ "Version 1 tried to add the following operation\n"
            ++ toErrorMessage v1Modification
            ++ "Version 2 tried to add the following operation\n"
            ++ toErrorMessage v2Modification
    where
      removeChangeHelper RemovedModification =
        "remove the operation\n"
      removeChangeHelper (ChangedModification newMod) =
        "change the operation to the following:\n"
          ++ toErrorMessage newMod

instance ConflictShow FeatureModification where
  toErrorMessage featureModification = show featureModification

instance ConflictShow GroupModification where
  toErrorMessage groupModification = show groupModification

instance ConflictShow LocalConflict where
  toErrorMessage localConflict = show localConflict

instance ConflictShow GlobalConflict where
  toErrorMessage globalConflict = show globalConflict

constructLocalConflict :: LocalConflict -> String
constructLocalConflict localConflict = show localConflict

constructGlobalConflict :: GlobalConflict -> String
constructGlobalConflict globalConflict = show globalConflict

-- data MergeConflict
--   = FeatureConflict (BothChange FeatureModification)
--   | GroupConflict (BothChange GroupModification)
--   deriving (Show, Eq, Read)

-- data LocalConflict
--   = FeatureAlreadyExists FeatureModification FeatureId
--   | FeatureNotExists FeatureModification FeatureId
--   | GroupAlreadyExists GroupModification GroupId
--   | GroupNotExists GroupModification GroupId
--   deriving (Show, Eq, Read)

-- data GlobalConflict
--   = FailedDependencies [Dependency]
--   deriving (Show, Eq, Read)

-- data Dependency
--   = FeatureDependency FeatureModification FeatureDependencyType
--   | GroupDependency GroupModification GroupDependencyType
--   deriving (Show, Eq, Read)

-- data FeatureDependencyType
--   = NoChildGroups FeatureId
--   | ParentGroupExists GroupId
--   | NoCycleFromFeature FeatureId
--   | FeatureIsWellFormed FeatureId
--   | UniqueName String
--   deriving (Show, Eq, Read)

-- data GroupDependencyType
--   = NoChildFeatures GroupId
--   | ParentFeatureExists FeatureId
--   | NoCycleFromGroup GroupId
--   | GroupIsWellFormed GroupId
--   deriving (Show, Eq, Read)
