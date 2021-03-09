module Merge.ChangeDetection where

import qualified Lenses as L
import Types

import Control.Lens
import qualified Data.Map as M
import qualified Data.Map.Merge.Lazy as Merge

------------------------------------------------------------------------
--                    Flatten Sound Evolution Plan                    --
------------------------------------------------------------------------

flattenSoundEvolutionPlan :: TreeUserEvolutionPlan -> FlatUserEvolutionPlan
flattenSoundEvolutionPlan =
  L.timePoints
    . traversed
    . L.featureModel
    %~ flattenSoundFeatureModel

flattenSoundFeatureModel :: TreeFeatureModel -> FlatFeatureModel
flattenSoundFeatureModel fm =
  FlatFeatureModel
    (fm ^. L.rootFeature . L.id)
    (M.fromList features)
    (M.fromList groups)
  where
    (features, groups) = flattenFeature Nothing (fm ^. L.rootFeature)
    flattenFeature mParentGroup (TreeFeature id featureType name groups) =
      ([(id, FlatFeature mParentGroup featureType name)], [])
        <> foldMap (flattenGroup id) groups
    flattenGroup parentFeature (TreeGroup id groupType features) =
      ([], [(id, FlatGroup parentFeature groupType)])
        <> foldMap (flattenFeature (Just id)) features

------------------------------------------------------------------------
--                     Derive Sound Modifications                     --
------------------------------------------------------------------------

deriveSoundModifications :: FlatUserEvolutionPlan -> FlatModificationEvolutionPlan
deriveSoundModifications (UserEvolutionPlan timePoints) = case timePoints of
  [] -> error "evolution plan has to have at least one time point!"
  ((TimePoint initialTime initialFM) : restTimePoints) ->
    TransformationEvolutionPlan
      initialTime
      initialFM
      (zipWith timePointsToPlan timePoints restTimePoints)

timePointsToPlan ::
  TimePoint FlatFeatureModel -> TimePoint FlatFeatureModel -> Plan Modifications
timePointsToPlan (TimePoint _ prevFM) (TimePoint currTime currFM) =
  Plan currTime $ diffFeatureModels prevFM currFM

-- diffFeatureModels will derive every modification
diffFeatureModels :: FlatFeatureModel -> FlatFeatureModel -> Modifications
diffFeatureModels prevFM currFM =
  Modifications
    ( calculateFeatureModifications
        (prevFM ^. L.features)
        (currFM ^. L.features)
    )
    ( calculateGroupModifications
        (prevFM ^. L.groups)
        (currFM ^. L.groups)
    )

calculateFeatureModifications ::
  M.Map FeatureId FlatFeature ->
  M.Map FeatureId FlatFeature ->
  M.Map FeatureId FeatureModification
calculateFeatureModifications =
  Merge.merge
    (Merge.mapMissing (const inPrevMissingNew))
    (Merge.mapMissing (const missingPrevInNew))
    (Merge.zipWithMaybeMatched (const inPrevInNew))
  where
    inPrevMissingNew _ = FeatureRemove
    missingPrevInNew (FlatFeature mParent featureType name) =
      case mParent of
        Nothing -> error "cannot add a new root"
        Just parent -> FeatureAdd parent featureType name
    inPrevInNew prev new =
      let FlatFeature prevParent prevFeatureType prevName = prev
          FlatFeature newParent newFeatureType newName = new
       in if prev == new
            then Nothing
            else
              Just $
                FeatureModification
                  ( case (prevParent, newParent) of
                      (Just prev, Just new)
                        | prev /= new ->
                          Just (FeatureParentModification new)
                      -- NOTE: since the root is assumed to never change,
                      -- we only record changes of non-root features
                      _ -> Nothing
                  )
                  ( if prevFeatureType == newFeatureType
                      then Nothing
                      else Just (FeatureTypeModification newFeatureType)
                  )
                  ( if prevName == newName
                      then Nothing
                      else Just (FeatureNameModification newName)
                  )

calculateGroupModifications ::
  M.Map GroupId FlatGroup ->
  M.Map GroupId FlatGroup ->
  M.Map GroupId GroupModification
calculateGroupModifications =
  Merge.merge
    (Merge.mapMissing (const inPrevMissingNew))
    (Merge.mapMissing (const missingPrevInNew))
    (Merge.zipWithMaybeMatched (const inPrevInNew))
  where
    inPrevMissingNew _ = GroupRemove
    missingPrevInNew (FlatGroup parent groupType) =
      GroupAdd parent groupType
    inPrevInNew prev new =
      let FlatGroup prevParent prevGroupType = prev
          FlatGroup newParent newGroupType = new
       in if prev == new
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
