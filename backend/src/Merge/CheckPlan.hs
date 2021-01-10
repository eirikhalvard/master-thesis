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
    integrateFeatures fm = foldlMOf (L.features . traversed) integrateFeature fm modifications
    integrateGroups fm = foldlMOf (L.groups . traversed) integrateGroup fm modifications

integrateFeature :: FeatureModel' -> FeatureModification -> WriterT [Dependency] (Either Conflict) FeatureModel'
integrateFeature fm featureModification = do
  tell [FeatureDependency featureModification (NoChildGroups "feature:placeholder:TODO")]
  return fm

integrateGroup :: FeatureModel' -> GroupModification -> WriterT [Dependency] (Either Conflict) FeatureModel'
integrateGroup fm groupModification = throwError $ Panic 42 "not implemented"

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
