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
integrateGroup time groupId fm groupModification = throwError $ Panic 42 "not implemented"

checkGlobalConflict ::
  [Dependency] ->
  TimePoint FeatureModel' ->
  Either Conflict (TimePoint FeatureModel')
checkGlobalConflict dependencies (TimePoint time featureModel) =
  undefined

------------------------------------------------------------------------
--                      Unflatten Evolution Plan                      --
------------------------------------------------------------------------

unflattenEvolutionPlan ::
  AbstractedLevelEvolutionPlan FeatureModel' ->
  Either Conflict (AbstractedLevelEvolutionPlan FeatureModel)
unflattenEvolutionPlan flatEvolutionPlan = undefined
