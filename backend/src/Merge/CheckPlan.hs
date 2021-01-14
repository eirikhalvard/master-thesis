module Merge.CheckPlan where

import qualified Lenses as L
import Merge.Types
import Types

import Control.Lens
import Control.Monad.Error.Class
import Control.Monad.Writer.Lazy
import qualified Data.Map as M
import qualified Data.Set as S

------------------------------------------------------------------------
--                    Integrate All Modifications                     --
------------------------------------------------------------------------

integrateAllModifications ::
  ModificationLevelEvolutionPlan FeatureModel' ->
  Either Conflict (AbstractedLevelEvolutionPlan FeatureModel')
integrateAllModifications evolutionPlan = case evolutionPlan of
  TransformationEvolutionPlan initialTime initialFM plans ->
    AbstractedLevelEvolutionPlan <$> scanEvolutionPlan plans (TimePoint initialTime initialFM)

scanEvolutionPlan ::
  [Plan Modifications] -> TimePoint FeatureModel' -> Either Conflict [TimePoint FeatureModel']
scanEvolutionPlan [] timePoint =
  return [timePoint]
scanEvolutionPlan (plan : plans) currentTimePoint = do
  (nextTimePointUnchecked, dependencies) <- runWriterT $ integrateSinglePlan plan currentTimePoint
  nextTimePoint <- checkGlobalConflict dependencies nextTimePointUnchecked
  convertedEvolutionPlan <- scanEvolutionPlan plans nextTimePoint
  return $ currentTimePoint : convertedEvolutionPlan

integrateSinglePlan ::
  Plan Modifications ->
  TimePoint FeatureModel' ->
  WriterT [Dependency] (Either Conflict) (TimePoint FeatureModel')
integrateSinglePlan (Plan nextTime modifications) (TimePoint prevTime featureModel) =
  TimePoint nextTime <$> newFeatureModel
  where
    newFeatureModel = integrateFeatures featureModel >>= integrateGroups
    integrateFeatures fm = ifoldlMOf (L.features . itraversed) (integrateFeature nextTime) fm modifications
    integrateGroups fm = ifoldlMOf (L.groups . itraversed) (integrateGroup nextTime) fm modifications

integrateFeature ::
  Time ->
  FeatureId ->
  FeatureModel' ->
  FeatureModification ->
  WriterT [Dependency] (Either Conflict) FeatureModel'
integrateFeature time featureId fm featureModification =
  case featureModification of
    FeatureAdd parentGroupId featureType name ->
      case M.lookup featureId (fm ^. L.features) of
        Nothing -> do
          tell . fmap (FeatureDependency featureModification) $
            [ ParentGroupExists parentGroupId
            , UniqueName name
            , FeatureIsWellFormed featureId
            ]
          return $ fm & L.features . at featureId ?~ Feature' (Just parentGroupId) featureType name
        Just oldFeature ->
          throwError $ Local time (FeatureAlreadyExists featureModification featureId)
    FeatureRemove ->
      case M.lookup featureId (fm ^. L.features) of
        Nothing ->
          throwError $ Local time (FeatureNotExists featureModification featureId)
        Just oldFeature -> do
          tell . fmap (FeatureDependency featureModification) $
            [NoChildGroups featureId]
          return $ fm & L.features . at featureId .~ Nothing
    FeatureModification parentGroupIdMod featureTypeMod nameMod ->
      if has (L.features . ix featureId) fm
        then
          pure fm
            >>= integrateParentMod
            >>= integrateTypeMod
            >>= integrateNameMod
        else
          throwError $
            Local time (FeatureNotExists featureModification featureId)
      where
        integrateParentMod :: FeatureModel' -> WriterT [Dependency] (Either Conflict) FeatureModel'
        integrateParentMod fm =
          case parentGroupIdMod of
            Nothing -> return fm
            Just (FeatureParentModification newValue) -> do
              tell . fmap (FeatureDependency featureModification) $
                [ ParentGroupExists newValue
                , NoCycleFromFeature featureId
                , FeatureIsWellFormed featureId
                ]
              return $ fm & L.features . ix featureId . L.parentGroupId .~ Just newValue

        integrateTypeMod :: FeatureModel' -> WriterT [Dependency] (Either Conflict) FeatureModel'
        integrateTypeMod fm =
          case featureTypeMod of
            Nothing -> return fm
            Just (FeatureTypeModification newValue) -> do
              tell . fmap (FeatureDependency featureModification) $
                [FeatureIsWellFormed featureId]
              return $ fm & L.features . ix featureId . L.featureType .~ newValue

        integrateNameMod :: FeatureModel' -> WriterT [Dependency] (Either Conflict) FeatureModel'
        integrateNameMod fm =
          case nameMod of
            Nothing -> return fm
            Just (FeatureNameModification newValue) -> do
              tell . fmap (FeatureDependency featureModification) $
                [UniqueName newValue]
              return $ fm & L.features . ix featureId . L.name .~ newValue

integrateGroup ::
  Time ->
  GroupId ->
  FeatureModel' ->
  GroupModification ->
  WriterT [Dependency] (Either Conflict) FeatureModel'
integrateGroup time groupId fm groupModification =
  case groupModification of
    GroupAdd parentFeatureId groupType ->
      case M.lookup groupId (fm ^. L.groups) of
        Nothing -> do
          tell . fmap (GroupDependency groupModification) $
            [ ParentFeatureExists parentFeatureId
            ]
          return $ fm & L.groups . at groupId ?~ Group' parentFeatureId groupType
        Just oldGroup ->
          throwError $ Local time (GroupAlreadyExists groupModification groupId)
    GroupRemove ->
      case M.lookup groupId (fm ^. L.groups) of
        Nothing ->
          throwError $ Local time (GroupNotExists groupModification groupId)
        Just oldGroup -> do
          tell . fmap (GroupDependency groupModification) $
            [NoChildFeatures groupId]
          return $ fm & L.groups . at groupId .~ Nothing
    GroupModification parentFeatureIdMod groupTypeMod ->
      if has (L.groups . ix groupId) fm
        then
          pure fm
            >>= integrateParentMod
            >>= integrateTypeMod
        else
          throwError $
            Local time (GroupNotExists groupModification groupId)
      where
        integrateParentMod :: FeatureModel' -> WriterT [Dependency] (Either Conflict) FeatureModel'
        integrateParentMod fm =
          case parentFeatureIdMod of
            Nothing -> return fm
            Just (GroupParentModification newValue) -> do
              tell . fmap (GroupDependency groupModification) $
                [ ParentFeatureExists newValue
                , NoCycleFromGroup groupId
                ]
              return $ fm & L.groups . ix groupId . L.parentFeatureId .~ newValue

        integrateTypeMod :: FeatureModel' -> WriterT [Dependency] (Either Conflict) FeatureModel'
        integrateTypeMod fm =
          case groupTypeMod of
            Nothing -> return fm
            Just (GroupTypeModification newValue) -> do
              tell . fmap (GroupDependency groupModification) $
                [GroupIsWellFormed groupId]
              return $ fm & L.groups . ix groupId . L.groupType .~ newValue

checkGlobalConflict ::
  [Dependency] ->
  TimePoint FeatureModel' ->
  Either Conflict (TimePoint FeatureModel')
checkGlobalConflict dependencies tp@(TimePoint time featureModel) =
  errorIfFailed . filter (not . checkDependency) $ dependencies
  where
    errorIfFailed failedDeps =
      case failedDeps of
        [] -> Right tp
        _ -> Left $ Global time (FailedDependencies failedDeps)
    checkDependency (FeatureDependency featureMod dependencyType) =
      case dependencyType of
        NoChildGroups featureId ->
          hasn't
            ( L.groups
                . traversed
                . L.parentFeatureId
                . filtered (== featureId)
            )
            featureModel
        ParentGroupExists groupId ->
          has
            (L.groups . ix groupId)
            featureModel
        NoCycleFromFeature featureId ->
          not $ featureInCycle S.empty featureId featureModel
        FeatureIsWellFormed featureId ->
          -- If feature is mandatory, parent has to be AND group
          -- === feature not mandatory or parent is and
          let featureType =
                featureModel
                  ^?! L.features
                    . ix featureId
                    . L.featureType
              parentGroupType =
                featureModel
                  ^?! L.parentGroupOfFeature featureId
                    . L.groupType
           in featureType /= Mandatory || parentGroupType == And
        UniqueName name ->
          lengthOf
            (L.features . traversed . L.name . filtered (== name))
            featureModel
            <= 1
    checkDependency (GroupDependency groupMod dependencyType) =
      case dependencyType of
        NoChildFeatures groupId ->
          hasn't
            ( L.features
                . traversed
                . L.parentGroupId
                . filtered (== Just groupId)
            )
            featureModel
        ParentFeatureExists featureId ->
          has
            (L.features . ix featureId)
            featureModel
        NoCycleFromGroup groupId ->
          not $ groupInCycle S.empty groupId featureModel
        GroupIsWellFormed groupId ->
          -- Either the group is a AND group, or all child features are optional
          let groupType = featureModel ^?! L.groups . ix groupId . L.groupType
              childFeatureTypes =
                featureModel
                  ^.. L.childFeaturesOfGroup groupId
                    . L.featureType
           in groupType == And || all (== Optional) childFeatureTypes

featureInCycle ::
  S.Set (Either FeatureId GroupId) ->
  FeatureId ->
  FeatureModel' ->
  Bool
featureInCycle visited featureId featureModel
  | Left featureId `elem` visited = True
  | otherwise =
    case featureModel
      ^? L.features
        . ix featureId
        . L.parentGroupId
        . _Just of
      Nothing -> False -- no parent group OR non existing feature
      Just parentGroupId ->
        groupInCycle
          (S.insert (Left featureId) visited)
          parentGroupId
          featureModel

groupInCycle ::
  S.Set (Either FeatureId GroupId) ->
  GroupId ->
  FeatureModel' ->
  Bool
groupInCycle visited groupId featureModel
  | Right groupId `elem` visited = True
  | otherwise =
    case featureModel
      ^? L.groups
        . ix groupId
        . L.parentFeatureId of
      Nothing -> False -- non existing group
      Just parentFeatureId ->
        featureInCycle
          (S.insert (Right groupId) visited)
          parentFeatureId
          featureModel

------------------------------------------------------------------------
--                      Unflatten Evolution Plan                      --
------------------------------------------------------------------------

unflattenEvolutionPlan ::
  AbstractedLevelEvolutionPlan FeatureModel' ->
  Either Conflict (AbstractedLevelEvolutionPlan FeatureModel)
unflattenEvolutionPlan =
  L.timePoints
    . traversed
    %%~ unflattenTimePoint

unflattenTimePoint :: TimePoint FeatureModel' -> Either Conflict (TimePoint FeatureModel)
unflattenTimePoint (TimePoint time featureModel) =
  TimePoint time . FeatureModel
    <$> unflattenFeature featureModel (featureModel ^. L.rootId)

unflattenFeature :: FeatureModel' -> FeatureId -> Either Conflict Feature
unflattenFeature featureModel featureId =
  Feature featureId featureType name <$> childGroupsM
  where
    childGroupIds = featureModel ^.. L.ichildGroupsOfFeature featureId . asIndex
    childGroupsM = S.fromList <$> traverse (unflattenGroup featureModel) childGroupIds
    (Feature' _ featureType name) = featureModel ^?! L.features . ix featureId

unflattenGroup :: FeatureModel' -> GroupId -> Either Conflict Group
unflattenGroup featureModel groupId =
  Group groupId groupType <$> childFeaturesM
  where
    childFeatureIds = featureModel ^.. L.ichildFeaturesOfGroup groupId . asIndex
    childFeaturesM = S.fromList <$> traverse (unflattenFeature featureModel) childFeatureIds
    (Group' _ groupType) = featureModel ^?! L.groups . ix groupId
