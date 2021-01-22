{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Lenses where

import Control.Lens
import Types

makeFieldsNoPrefix ''TreeFeatureModel
makeFieldsNoPrefix ''TreeFeature
makeFieldsNoPrefix ''TreeGroup
makeFieldsNoPrefix ''FlatFeatureModel
makeFieldsNoPrefix ''FlatFeature
makeFieldsNoPrefix ''FlatGroup
makePrisms ''FeatureType
makePrisms ''GroupType

-- Using makeLenses in order to get polymorphic lenses
makeLenses ''UserEvolutionPlan
makeLenses ''TimePoint
makeLenses ''TransformationEvolutionPlan
makeLenses ''Plan

makeFieldsNoPrefix ''Modifications
makePrisms ''FeatureModification
makeFieldsNoPrefix ''FeatureParentModification
makeFieldsNoPrefix ''FeatureNameModification
makeFieldsNoPrefix ''FeatureTypeModification
makePrisms ''GroupModification
makeFieldsNoPrefix ''GroupParentModification
makeFieldsNoPrefix ''GroupTypeModification

makeFieldsNoPrefix ''DiffResult
makePrisms ''SingleDiffResult
makePrisms ''OneChange
makePrisms ''BothChange
makePrisms ''RemovedOrChangedModification
makeFieldsNoPrefix ''AddedModification
makePrisms ''Version

makeFieldsNoPrefix ''MergeInput
makeFieldsNoPrefix ''MergeInputWithExpected

makeFieldsNoPrefix ''ElmDataExamples
makeFieldsNoPrefix ''ElmMergeExample
makeFieldsNoPrefix ''ElmNamedEvolutionPlan

-- --- Optic helpers

parentGroupOfFeature :: FeatureId -> Traversal' FlatFeatureModel FlatGroup
parentGroupOfFeature fid handler fm =
  case fm ^? features . ix fid . parentGroupId . _Just of
    Nothing -> pure fm
    Just parentGroupIdValue ->
      traverseOf
        (groups . ix parentGroupIdValue)
        handler
        fm

parentFeatureOfGroup :: GroupId -> Traversal' FlatFeatureModel FlatFeature
parentFeatureOfGroup gid handler fm =
  case fm ^? groups . ix gid . parentFeatureId of
    Nothing -> pure fm
    Just parentFeatureIdValue ->
      traverseOf
        (features . ix parentFeatureIdValue)
        handler
        fm

childGroupsOfFeature :: FeatureId -> Traversal' FlatFeatureModel FlatGroup
childGroupsOfFeature fid =
  groups . traversed . filtered ((fid ==) . view parentFeatureId)

ichildGroupsOfFeature :: FeatureId -> IndexedTraversal' GroupId FlatFeatureModel FlatGroup
ichildGroupsOfFeature fid =
  groups . itraversed . filtered ((fid ==) . view parentFeatureId)

childFeaturesOfGroup :: GroupId -> Traversal' FlatFeatureModel FlatFeature
childFeaturesOfGroup gid =
  features . traversed . filtered ((Just gid ==) . view parentGroupId)

ichildFeaturesOfGroup :: GroupId -> IndexedTraversal' FeatureId FlatFeatureModel FlatFeature
ichildFeaturesOfGroup gid =
  features . itraversed . filtered ((Just gid ==) . view parentGroupId)
