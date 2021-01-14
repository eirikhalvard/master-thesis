{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Lenses where

import Control.Lens
import qualified Data.Map as M
import Types

makeFieldsNoPrefix ''FeatureModel
makeFieldsNoPrefix ''Feature
makeFieldsNoPrefix ''Group
makeFieldsNoPrefix ''FeatureModel'
makeFieldsNoPrefix ''Feature'
makeFieldsNoPrefix ''Group'
makePrisms ''FeatureType
makePrisms ''GroupType

-- Using makeLenses in order to get polymorphic lenses
makeLenses ''AbstractedLevelEvolutionPlan
makeLenses ''TimePoint
makeLenses ''TransformationEvolutionPlan
makeLenses ''Plan

makePrisms ''Operation
makeFieldsNoPrefix ''AddFeatureOp
makeFieldsNoPrefix ''RemoveFeatureOp
makeFieldsNoPrefix ''MoveFeatureOp
makeFieldsNoPrefix ''RenameFeatureOp
makeFieldsNoPrefix ''ChangeFeatureTypeOp
makeFieldsNoPrefix ''AddGroupOp
makeFieldsNoPrefix ''RemoveGroupOp
makeFieldsNoPrefix ''ChangeGroupTypeOp
makeFieldsNoPrefix ''MoveGroupOp

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

makeFieldsNoPrefix ''MergeEvolutionPlan
makeFieldsNoPrefix ''MergeResult

-- --- Optic helpers

parentGroupOfFeature :: FeatureId -> Traversal' FeatureModel' Group'
parentGroupOfFeature fid handler fm =
  case fm ^? features . ix fid . parentGroupId . _Just of
    Nothing -> pure fm
    Just parentGroupIdValue ->
      traverseOf
        (groups . ix parentGroupIdValue)
        handler
        fm

parentFeatureOfGroup :: GroupId -> Traversal' FeatureModel' Feature'
parentFeatureOfGroup gid handler fm =
  case fm ^? groups . ix gid . parentFeatureId of
    Nothing -> pure fm
    Just parentFeatureIdValue ->
      traverseOf
        (features . ix parentFeatureIdValue)
        handler
        fm

childGroupsOfFeature :: FeatureId -> Traversal' FeatureModel' Group'
childGroupsOfFeature fid =
  groups . traversed . filtered ((fid ==) . view parentFeatureId)

ichildGroupsOfFeature :: FeatureId -> IndexedTraversal' GroupId FeatureModel' Group'
ichildGroupsOfFeature fid =
  groups . itraversed . filtered ((fid ==) . view parentFeatureId)

childFeaturesOfGroup :: GroupId -> Traversal' FeatureModel' Feature'
childFeaturesOfGroup gid =
  features . traversed . filtered ((Just gid ==) . view parentGroupId)

ichildFeaturesOfGroup :: GroupId -> IndexedTraversal' FeatureId FeatureModel' Feature'
ichildFeaturesOfGroup gid =
  features . itraversed . filtered ((Just gid ==) . view parentGroupId)
