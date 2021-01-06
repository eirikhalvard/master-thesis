module Merge.ChangeDetection where

import qualified Lenses as L
import Types

import Control.Lens
import qualified Data.Map as M
import qualified Data.Map.Merge.Lazy as Merge

------------------------------------------------------------------------
--                       Flatten Evolution Plan                       --
------------------------------------------------------------------------

flattenEvolutionPlan ::
  AbstractedLevelEvolutionPlan FeatureModel ->
  AbstractedLevelEvolutionPlan FeatureModel'
flattenEvolutionPlan =
  L.timePoints
    . traversed
    . L.featureModel
    %~ flattenFeatureModel

flattenFeatureModel :: FeatureModel -> FeatureModel'
flattenFeatureModel fm =
  FeatureModel'
    (fm ^. L.rootFeature . L.id)
    (M.fromList features)
    (M.fromList groups)
  where
    (features, groups) = flattenFeature Nothing (fm ^. L.rootFeature)
    flattenFeature mParentGroup (Feature id featureType name groups) =
      ([(id, Feature' mParentGroup featureType name)], [])
        <> foldMap (flattenGroup id) groups
    flattenGroup parentFeature (Group id groupType features) =
      ([], [(id, Group' parentFeature groupType)])
        <> foldMap (flattenFeature (Just id)) features

------------------------------------------------------------------------
--            Construct Modification Level Evolution Plan             --
------------------------------------------------------------------------

constructModificationLevelEP ::
  AbstractedLevelEvolutionPlan FeatureModel' ->
  ModificationLevelEvolutionPlan FeatureModel'
constructModificationLevelEP (AbstractedLevelEvolutionPlan timePoints) = case timePoints of
  [] -> error "evolution plan has to have at least one time point!"
  ((TimePoint initialTime initialFM) : restTimePoints) ->
    TransformationEvolutionPlan
      initialTime
      initialFM
      (zipWith timePointsToPlan timePoints restTimePoints)

timePointsToPlan ::
  TimePoint FeatureModel' -> TimePoint FeatureModel' -> Plan Modifications
timePointsToPlan (TimePoint _ prevFM) (TimePoint currTime currFM) =
  Plan currTime $ diffFeatureModels prevFM currFM

-- diffFeatureModels will derive every modification
diffFeatureModels :: FeatureModel' -> FeatureModel' -> Modifications
diffFeatureModels prevFM currFM =
  Modifications
    featureModifications
    groupModifications
  where
    featureModifications =
      Merge.merge
        (Merge.mapMissing (\_ _ -> FeatureRemove))
        ( Merge.mapMissing
            ( \_ (Feature' parent featureType name) ->
                FeatureAdd parent featureType name
            )
        )
        ( Merge.zipWithMaybeMatched
            ( \_
               prev@(Feature' prevParent prevFeatureType prevName)
               new@(Feature' newParent newFeatureType newName) ->
                  if prev == new
                    then Nothing
                    else
                      Just $
                        FeatureModification
                          ( if prevParent == newParent
                              then Nothing
                              else Just (FeatureParentModification newParent)
                          )
                          ( if prevFeatureType == newFeatureType
                              then Nothing
                              else Just (FeatureTypeModification newFeatureType)
                          )
                          ( if prevName == newName
                              then Nothing
                              else Just (FeatureNameModification newName)
                          )
            )
        )
        (prevFM ^. L.features)
        (currFM ^. L.features)
    groupModifications =
      Merge.merge
        (Merge.mapMissing (\_ _ -> GroupRemove))
        ( Merge.mapMissing
            ( \_ (Group' parent groupType) ->
                GroupAdd parent groupType
            )
        )
        ( Merge.zipWithMaybeMatched
            ( \_
               prev@(Group' prevParent prevGroupType)
               new@(Group' newParent newGroupType) ->
                  if prev == new
                    then Nothing
                    else
                      Just $
                        GroupModification
                          ( if prevParent == newParent
                              then Nothing
                              else Just (GroupParentModification newParent)
                          )
                          ( if prevGroupType == newGroupType
                              then Nothing
                              else Just (GroupTypeModification newGroupType)
                          )
            )
        )
        (prevFM ^. L.groups)
        (currFM ^. L.groups)
