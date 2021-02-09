{-# LANGUAGE FlexibleInstances #-}

module Conflict where

import Data.List (intercalate)
import Data.Maybe (catMaybes)

import Types

------------------------------------------------------------------------
--                      CONFLICT ERROR MESSAGES                       --
------------------------------------------------------------------------

class ConflictShow a where
  toErrorMessage :: a -> String

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

constructErrorMsg :: Int -> String -> String
constructErrorMsg time reason =
  "A conflict occured at time\n"
    ++ show time
    ++ "\n---\n"
    ++ "The conflict occured because of "
    ++ reason

------------------------------------------------------------------------
--                           MERGE CONFLICT                           --
------------------------------------------------------------------------

instance ConflictShow MergeConflict where
  toErrorMessage (FeatureConflict id bothChange) =
    toErrorMessage (id, bothChange)
  toErrorMessage (GroupConflict id bothChange) =
    toErrorMessage (id, bothChange)

instance
  ConflictShow modificationType =>
  ConflictShow (String, BothChange modificationType)
  where
  toErrorMessage (id, bothChange) =
    case bothChange of
      BothChangeWithBase
        baseModification
        removedOrChangedV1
        removedOrChangedV2 ->
          "conflicting operations for "
            ++ id
            ++ "\n"
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
            ++ "\n"
            ++ "Version 2 tried to add the following operation\n"
            ++ toErrorMessage v2Modification
            ++ "\n"
    where
      removeChangeHelper RemovedModification =
        "remove the operation\n"
      removeChangeHelper (ChangedModification newMod) =
        "change the operation to the following:\n"
          ++ toErrorMessage newMod

instance ConflictShow FeatureModification where
  toErrorMessage featureModification =
    case featureModification of
      FeatureAdd parentGroupId featureType name ->
        "Feature Addition"
          ++ ": parent = "
          ++ parentGroupId
          ++ ", type = "
          ++ show featureType
          ++ ", name = "
          ++ name
      FeatureRemove -> "Feature Removal"
      FeatureModification mParent mType mName ->
        "Feature Modification: "
          ++ ( intercalate ", " . catMaybes $
                [ fmap (("parent = " ++) . toErrorMessage) mParent
                , fmap (("type = " ++) . toErrorMessage) mType
                , fmap (("name = " ++) . toErrorMessage) mName
                ]
             )

instance ConflictShow FeatureParentModification where
  toErrorMessage (FeatureParentModification parent) = parent

instance ConflictShow FeatureTypeModification where
  toErrorMessage (FeatureTypeModification featureType) = show featureType

instance ConflictShow FeatureNameModification where
  toErrorMessage (FeatureNameModification featureName) = featureName

instance ConflictShow GroupModification where
  toErrorMessage groupModification =
    case groupModification of
      GroupAdd parentGroupId groupType ->
        "Group Addition"
          ++ ": parent = "
          ++ parentGroupId
          ++ ", type = "
          ++ show groupType
      GroupRemove -> "Group Removal"
      GroupModification mParent mType ->
        "Group Modification: "
          ++ ( intercalate ", " . catMaybes $
                [ fmap (("parent = " ++) . toErrorMessage) mParent
                , fmap (("type = " ++) . toErrorMessage) mType
                ]
             )

instance ConflictShow GroupParentModification where
  toErrorMessage (GroupParentModification groupParent) = groupParent

instance ConflictShow GroupTypeModification where
  toErrorMessage (GroupTypeModification groupType) = show groupType

instance ConflictShow LocalConflict where
  toErrorMessage localConflict = show localConflict

instance ConflictShow GlobalConflict where
  toErrorMessage globalConflict = show globalConflict

------------------------------------------------------------------------
--                           LOCAL CONFLICT                           --
------------------------------------------------------------------------

constructLocalConflict :: LocalConflict -> String
constructLocalConflict localConflict = show localConflict

-- data LocalConflict
--   = FeatureAlreadyExists FeatureModification FeatureId
--   | FeatureNotExists FeatureModification FeatureId
--   | GroupAlreadyExists GroupModification GroupId
--   | GroupNotExists GroupModification GroupId
--   deriving (Show, Eq, Read)

------------------------------------------------------------------------
--                          GLOBAL CONFLICT                           --
------------------------------------------------------------------------

constructGlobalConflict :: GlobalConflict -> String
constructGlobalConflict globalConflict = show globalConflict

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
