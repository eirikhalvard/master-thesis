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
    ++ "The conflict occured because "
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
          "of conflicting operations for "
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
          "of conflicting operations for "
            ++ id
            ++ "\n"
            ++ "Version 1 tried to add the following operation\n"
            ++ toErrorMessage v1Modification
            ++ "\n"
            ++ "Version 2 tried to add the following operation\n"
            ++ toErrorMessage v2Modification
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
          ++ show parentGroupId
          ++ ", type = "
          ++ show featureType
          ++ ", name = "
          ++ show name
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
  toErrorMessage (FeatureParentModification parent) = show parent

instance ConflictShow FeatureTypeModification where
  toErrorMessage (FeatureTypeModification featureType) = show featureType

instance ConflictShow FeatureNameModification where
  toErrorMessage (FeatureNameModification featureName) = show featureName

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
  toErrorMessage (GroupParentModification groupParent) = show groupParent

instance ConflictShow GroupTypeModification where
  toErrorMessage (GroupTypeModification groupType) = show groupType

------------------------------------------------------------------------
--                           LOCAL CONFLICT                           --
------------------------------------------------------------------------

instance ConflictShow LocalConflict where
  toErrorMessage localConflict =
    "the following modification could not be integrated into the merged plan\n"
      ++ reason
    where
      reason = case localConflict of
        FeatureAlreadyExists featureModification featureId ->
          toErrorMessage featureModification
            ++ "\non feature id "
            ++ featureId
            ++ "\nbecause the feature already exists!"
        FeatureNotExists featureModification featureId ->
          toErrorMessage featureModification
            ++ "\non feature id "
            ++ featureId
            ++ "\nbecause the feature does not exist!"
        GroupAlreadyExists groupModification groupId ->
          toErrorMessage groupModification
            ++ "\non group id "
            ++ groupId
            ++ "\nbecause the group already exists!"
        GroupNotExists groupModification groupId ->
          toErrorMessage groupModification
            ++ "\non group id "
            ++ groupId
            ++ "\nbecause the group does not exist!"

------------------------------------------------------------------------
--                          GLOBAL CONFLICT                           --
------------------------------------------------------------------------

instance ConflictShow GlobalConflict where
  toErrorMessage globalConflict =
    case globalConflict of
      FailedDependencies dependencies ->
        "the resulting feature model was unsound\n"
          ++ "All the modification at the timepoint could merge, "
          ++ "but one or more of the modifications lead to conflicts in the feature model:"
          ++ concatMap (("\n\n" ++) . toErrorMessage) dependencies

-- TODO: add feature/group id to type
instance ConflictShow Dependency where
  toErrorMessage dependency =
    "the following modification could not be integrated into the merged plan\n"
      ++ modificationError
      ++ "\n"
      ++ reason
    where
      modificationError =
        case dependency of
          FeatureDependency featureModification _ ->
            toErrorMessage featureModification
          GroupDependency groupModification _ ->
            toErrorMessage groupModification
      reason =
        case dependency of
          FeatureDependency _ (NoChildGroups featureId) ->
            "because feature with id "
              ++ show featureId
              ++ " has one or more child groups"
          FeatureDependency _ (ParentGroupExists groupId) ->
            "because the features parent group "
              ++ show groupId
              ++ " does not exist"
          FeatureDependency _ (NoCycleFromFeature featureId) ->
            "because the feature with id "
              ++ show featureId
              ++ " forms a cycle"
          FeatureDependency _ (FeatureIsWellFormed featureId) ->
            "because the feature with id "
              ++ show featureId
              ++ " is not well formed"
          FeatureDependency _ (UniqueName name) ->
            "because the name "
              ++ show name
              ++ " is not unique"
          GroupDependency _ (NoChildFeatures groupId) ->
            "because group with id "
              ++ show groupId
              ++ " has one or more child features"
          GroupDependency _ (ParentFeatureExists featureId) ->
            "because the groups parent feature "
              ++ show featureId
              ++ " does not exist"
          GroupDependency _ (NoCycleFromGroup groupId) ->
            "because the group with id "
              ++ show groupId
              ++ " forms a cycle"
          GroupDependency _ (GroupIsWellFormed groupId) ->
            "because the group with id "
              ++ show groupId
              ++ " is not well formed"
