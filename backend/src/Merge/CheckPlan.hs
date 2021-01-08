module Merge.CheckPlan where

import qualified Lenses as L
import Merge.Types
import Types

import Control.Lens
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
  nextTimePoint <- integrateSinglePlan plan currentTimePoint >>= checkGlobalConflict
  convertedEvolutionPlan <- scanEvolutionPlan plans nextTimePoint
  return $ currentTimePoint : convertedEvolutionPlan

integrateSinglePlan ::
  Plan Modifications ->
  TimePoint FeatureModel' ->
  Either Conflict (TimePoint FeatureModel')
integrateSinglePlan (Plan nextTime modifications) (TimePoint prevTime featureModel) =
  TimePoint nextTime <$> newFeatureModel
  where
    newFeatureModel = integrateFeatures featureModel >>= integrateGroups
    integrateFeatures fm = foldlMOf (L.features . traversed) integrateFeature fm modifications
    integrateGroups fm = foldlMOf (L.groups . traversed) integrateGroup fm modifications

integrateFeature :: FeatureModel' -> FeatureModification -> Either Conflict FeatureModel'
integrateFeature fm featureModification = Right fm

integrateGroup :: FeatureModel' -> GroupModification -> Either Conflict FeatureModel'
integrateGroup fm groupModification = Left $ Panic 42 "not implemented"

checkGlobalConflict ::
  TimePoint FeatureModel' ->
  Either Conflict (TimePoint FeatureModel')
checkGlobalConflict (TimePoint time featureModel) =
  undefined

------------------------------------------------------------------------
--                      Unflatten Evolution Plan                      --
------------------------------------------------------------------------

unflattenEvolutionPlan ::
  AbstractedLevelEvolutionPlan FeatureModel' ->
  Either Conflict (AbstractedLevelEvolutionPlan FeatureModel)
unflattenEvolutionPlan flatEvolutionPlan = undefined
