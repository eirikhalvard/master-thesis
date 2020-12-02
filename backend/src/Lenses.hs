{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Lenses where

import Control.Lens
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
