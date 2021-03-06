{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Types where

import qualified Data.Map as M
import qualified Data.Set as S

import Data.Aeson
import Deriving.Aeson
import Deriving.Aeson.Stock
import GHC.Generics

------------------------------------------------------------------------
--                           Feature Models                           --
------------------------------------------------------------------------

type FeatureId = String

type GroupId = String

--- Tree Structured Feature Model ---

data TreeFeatureModel = TreeFeatureModel
  { _rootFeature :: TreeFeature
  }
  deriving (Show, Eq, Read, Generic)
  deriving
    (FromJSON, ToJSON)
    via Prefixed "_" TreeFeatureModel

data TreeFeature = TreeFeature
  { _id :: FeatureId
  , _featureType :: FeatureType
  , _name :: String
  , _groups :: S.Set TreeGroup
  }
  deriving (Show, Eq, Read, Ord, Generic)
  deriving
    (FromJSON, ToJSON)
    via Prefixed "_" TreeFeature

data TreeGroup = TreeGroup
  { _id :: GroupId
  , _groupType :: GroupType
  , _features :: S.Set TreeFeature
  }
  deriving (Show, Eq, Read, Ord, Generic)
  deriving
    (FromJSON, ToJSON)
    via Prefixed "_" TreeGroup

--- Flat Structured Feature Model ---

data FlatFeatureModel = FlatFeatureModel
  { _rootId :: FeatureId
  , _features :: M.Map FeatureId FlatFeature
  , _groups :: M.Map GroupId FlatGroup
  }
  deriving (Show, Eq, Read, Generic)
  deriving
    (FromJSON, ToJSON)
    via Prefixed "_" FlatFeatureModel

data FlatFeature = FlatFeature
  { _parentGroupId :: Maybe GroupId
  , _featureType :: FeatureType
  , _name :: String
  }
  deriving (Show, Eq, Read, Generic)
  deriving
    (FromJSON, ToJSON)
    via Prefixed "_" FlatFeature

data FlatGroup = FlatGroup
  { _parentFeatureId :: FeatureId
  , _groupType :: GroupType
  }
  deriving (Show, Eq, Read, Generic)
  deriving
    (FromJSON, ToJSON)
    via Prefixed "_" FlatGroup

data FeatureType
  = Optional
  | Mandatory
  deriving (Show, Eq, Read, Ord, Generic)
  deriving
    (FromJSON, ToJSON)
    via Prefixed "_" FeatureType

data GroupType
  = And
  | Or
  | Alternative
  deriving (Show, Eq, Read, Ord, Generic)
  deriving
    (FromJSON, ToJSON)
    via Prefixed "_" GroupType

------------------------------------------------------------------------
--                          Evolution Plans                           --
------------------------------------------------------------------------

--  Four different types of evolution plan representations. We categorize them in
--  two categories. User evolution plans and Tranformation evolution plans
--
--    User Evolution Plans:
--      Represents the evolution plan as a list of feature models, where each
--      feature model is coupled with a time point. In this representation the
--      exact changes between each feature model is implicit as the difference
--      between each pair of feature models
--    Transformation Evolution Plans:
--      Represents the evolution plan as an initial model, together with a list
--      of plans, where each plan is a time point and a transformation. The
--      transformation describes how the previous feature model should be
--      transformed in order to achieve the feature model at the given time
--      point. We define two different types of transformations, namely
--      Modification level and merge level modifications.
--
--      Modification  Transformation:
--        Represents the transformation as a set of modifications. This
--        representation guarantees that each there are no conflicting
--        modifications, i.e. moving a feature twice. This allows for merging
--        the modifications in an arbitrary ordering, since no modifications
--        shadow others, etc.
--      Merge  Transformation:
--        The merge level transformation represents the "planned"
--        transformations from both versions in the merge. The transformation
--        is essentially the union of the modifications of version 1 and
--        version 2. In this representation, a feature might be planned to be
--        changed, added or removed in several versions, which this
--        representation encodes.

type TreeUserEvolutionPlan = UserEvolutionPlan TreeFeatureModel

type FlatUserEvolutionPlan = UserEvolutionPlan FlatFeatureModel

type FlatModificationEvolutionPlan = ModificationEvolutionPlan FlatFeatureModel

type Time = Int

data UserEvolutionPlan featureModel = UserEvolutionPlan
  { _timePoints :: [TimePoint featureModel]
  }
  deriving (Show, Eq, Read, Generic)
  deriving
    (FromJSON, ToJSON)
    via Prefixed "_" (UserEvolutionPlan featureModel)

data TimePoint featureModel = TimePoint
  { _time :: Time
  , _featureModel :: featureModel
  }
  deriving (Show, Eq, Read, Generic)
  deriving
    (FromJSON, ToJSON)
    via Prefixed "_" (TimePoint featureModel)

data TransformationEvolutionPlan transformation featureModel = TransformationEvolutionPlan
  { _initialTime :: Time
  , _initialFM :: featureModel
  , _plans :: [Plan transformation]
  }
  deriving (Show, Eq, Read, Generic)
  deriving
    (FromJSON, ToJSON)
    via Prefixed "_" (TransformationEvolutionPlan transformation featureModel)

data Plan transformation = Plan
  { _timePoint :: Time
  , _transformation :: transformation
  }
  deriving (Show, Eq, Read, Generic)
  deriving
    (FromJSON, ToJSON)
    via Prefixed "_" (Plan transformation)

type ModificationEvolutionPlan featureModel = TransformationEvolutionPlan Modifications featureModel

type MergeEvolutionPlan featureModel = TransformationEvolutionPlan DiffResult featureModel

------------------------------------------------------------------------
--                        Transformation Types                        --
------------------------------------------------------------------------

--- MODIFICATIONS ---

-- Modifications vs Changes
-- We have two levels of changes. To differentiate between the two, we will use
-- the name Modification or Change in order to separate the two
--
-- Modifications:
--   Modifications are the actual changes between two feature models. For
--   example, If a feature was removed or added, we will call this "change" as
--   a Modification
--
-- Changes:
--   Changes are relevant to the diff-algorithm and its output, and refer to
--   the meta-level changes on modifications.  If a base version included
--   a Modification, i.e. an addition of a feature, one of the derived versions
--   could remove this modification The derived version has then Changed
--   a modification. So Change-names is reserved for these meta-level changes

--- Modifications between featuremodels ---

data Modifications = Modifications
  { _features :: M.Map FeatureId FeatureModification
  , _groups :: M.Map GroupId GroupModification
  }
  deriving (Show, Eq, Read, Generic)
  deriving
    (FromJSON, ToJSON)
    via Prefixed "_" Modifications

data FeatureModification
  = FeatureAdd GroupId FeatureType String
  | FeatureRemove
  | FeatureModification
      (Maybe FeatureParentModification)
      (Maybe FeatureTypeModification)
      (Maybe FeatureNameModification)
  deriving (Show, Eq, Read, Generic)
  deriving
    (FromJSON, ToJSON)
    via Prefixed "_" FeatureModification

data FeatureParentModification
  = FeatureParentModification GroupId
  deriving (Show, Eq, Read, Generic)
  deriving
    (FromJSON, ToJSON)
    via Prefixed "_" FeatureParentModification

data FeatureNameModification
  = FeatureNameModification String
  deriving (Show, Eq, Read, Generic)
  deriving
    (FromJSON, ToJSON)
    via Prefixed "_" FeatureNameModification

data FeatureTypeModification
  = FeatureTypeModification FeatureType
  deriving (Show, Eq, Read, Generic)
  deriving
    (FromJSON, ToJSON)
    via Prefixed "_" FeatureTypeModification

data GroupModification
  = GroupAdd FeatureId GroupType
  | GroupRemove
  | GroupModification
      (Maybe GroupParentModification)
      (Maybe GroupTypeModification)
  deriving (Show, Eq, Read, Generic)
  deriving
    (FromJSON, ToJSON)
    via Prefixed "_" GroupModification

data GroupParentModification
  = GroupParentModification FeatureId
  deriving (Show, Eq, Read, Generic)
  deriving
    (FromJSON, ToJSON)
    via Prefixed "_" GroupParentModification

data GroupTypeModification
  = GroupTypeModification GroupType
  deriving (Show, Eq, Read, Generic)
  deriving
    (FromJSON, ToJSON)
    via Prefixed "_" GroupTypeModification

--- DIFF RESULT ---

-- The diff result from the all the changes in the entire time point for all
-- versions of the model
data DiffResult = DiffResult
  { _features :: M.Map FeatureId FeatureDiffResult
  , _groups :: M.Map GroupId GroupDiffResult
  }
  deriving (Show, Eq, Read, Generic)
  deriving
    (FromJSON, ToJSON)
    via Prefixed "_" DiffResult

type FeatureDiffResult =
  SingleDiffResult FeatureModification

type GroupDiffResult =
  SingleDiffResult GroupModification

-- Every possible combination that a feature- or group change could be modified
data SingleDiffResult modificationType
  = NoChange modificationType
  | ChangedInOne Version (OneChange modificationType)
  | ChangedInBoth (BothChange modificationType)
  deriving (Show, Eq, Read, Generic)
  deriving
    (FromJSON, ToJSON)
    via Prefixed "_" (SingleDiffResult modificationType)

data OneChange modificationType
  = OneChangeWithBase
      modificationType -- Base modification
      (RemovedOrChangedModification modificationType) -- Derived (V1 or V2) modification
  | OneChangeWithoutBase
      (AddedModification modificationType) -- Derived (V1 or V2) modification
  deriving (Show, Eq, Read, Generic)
  deriving
    (FromJSON, ToJSON)
    via Prefixed "_" (OneChange modificationType)

data BothChange modificationType
  = BothChangeWithBase
      modificationType -- Base modification
      (RemovedOrChangedModification modificationType) -- V1 modification
      (RemovedOrChangedModification modificationType) -- V2 modification
  | BothChangeWithoutBase
      (AddedModification modificationType) -- V1 modification
      (AddedModification modificationType) -- V2 modification
  deriving (Show, Eq, Read, Generic)
  deriving
    (FromJSON, ToJSON)
    via Prefixed "_" (BothChange modificationType)

data RemovedOrChangedModification modificationType
  = RemovedModification
  | ChangedModification modificationType
  deriving (Show, Eq, Read, Generic)
  deriving
    (FromJSON, ToJSON)
    via Prefixed "_" (RemovedOrChangedModification modificationType)

data AddedModification modificationType
  = AddedModification modificationType
  deriving (Show, Eq, Read, Generic)
  deriving
    (FromJSON, ToJSON)
    via Prefixed "_" (AddedModification modificationType)

data Version
  = V1
  | V2
  deriving (Show, Eq, Read, Generic)
  deriving
    (FromJSON, ToJSON)
    via Prefixed "_" Version

------------------------------------------------------------------------
--                       Merge Input / Output                         --
------------------------------------------------------------------------

data MergeInput
  = TreeUser (MergeInputData TreeUserEvolutionPlan)
  | FlatUser (MergeInputData FlatUserEvolutionPlan)
  | FlatModification (MergeInputData FlatModificationEvolutionPlan)
  deriving (Show, Eq, Read)

data MergeInputData evolutionPlan = MergeInputData
  { _name :: String
  , _base :: evolutionPlan
  , _v1 :: evolutionPlan
  , _v2 :: evolutionPlan
  , _maybeExpected :: Maybe (MergeResult evolutionPlan)
  }
  deriving (Show, Eq, Read, Generic, Functor)
  deriving
    (FromJSON, ToJSON)
    via Prefixed "_" (MergeInputData evolutionPlan)

type MergeOutput = Either Conflict (FlatModificationEvolutionPlan, FlatUserEvolutionPlan)

type MergeResult evolutionPlan = Either Conflict evolutionPlan

------------------------------------------------------------------------
--                       Elm Data Serialization                       --
------------------------------------------------------------------------

data ElmDataExamples = ElmDataExamples
  { _examples :: [ElmMergeExample]
  }
  deriving (Show, Eq, Read, Generic)
  deriving
    (FromJSON, ToJSON)
    via Prefixed "_" ElmDataExamples

data ElmMergeExample = ElmMergeExample
  { _name :: String
  , _evolutionPlans :: [ElmNamedEvolutionPlan]
  }
  deriving (Show, Eq, Read, Generic)
  deriving
    (FromJSON, ToJSON)
    via Prefixed "_" ElmMergeExample

data ElmNamedEvolutionPlan = ElmNamedEvolutionPlan
  { _name :: String
  , _mergeData :: Either String TreeUserEvolutionPlan
  }
  deriving (Show, Eq, Read, Generic)
  deriving
    (FromJSON, ToJSON)
    via Prefixed "_" ElmNamedEvolutionPlan

------------------------------------------------------------------------
--                              Conflict                              --
------------------------------------------------------------------------

data Conflict
  = Merge Time MergeConflict
  | Local Time LocalConflict
  | Global Time GlobalConflict
  | Panic Time String
  deriving (Show, Eq, Read, Generic)
  deriving
    (FromJSON, ToJSON)
    via Prefixed "_" Conflict

data MergeConflict
  = FeatureConflict FeatureId (BothChange FeatureModification)
  | GroupConflict GroupId (BothChange GroupModification)
  deriving (Show, Eq, Read, Generic)
  deriving
    (FromJSON, ToJSON)
    via Prefixed "_" MergeConflict

data LocalConflict
  = FeatureAlreadyExists FeatureModification FeatureId
  | FeatureNotExists FeatureModification FeatureId
  | GroupAlreadyExists GroupModification GroupId
  | GroupNotExists GroupModification GroupId
  deriving (Show, Eq, Read, Generic)
  deriving
    (FromJSON, ToJSON)
    via Prefixed "_" LocalConflict

data GlobalConflict
  = FailedDependencies [Dependency]
  deriving (Show, Eq, Read, Generic)
  deriving
    (FromJSON, ToJSON)
    via Prefixed "_" GlobalConflict

data Dependency
  = FeatureDependency FeatureModification FeatureDependencyType
  | GroupDependency GroupModification GroupDependencyType
  deriving (Show, Eq, Read, Generic)
  deriving
    (FromJSON, ToJSON)
    via Prefixed "_" Dependency

data FeatureDependencyType
  = NoChildGroups FeatureId
  | ParentGroupExists GroupId
  | NoCycleFromFeature FeatureId
  | FeatureIsWellFormed FeatureId
  | UniqueName String
  deriving (Show, Eq, Read, Generic)
  deriving
    (FromJSON, ToJSON)
    via Prefixed "_" FeatureDependencyType

data GroupDependencyType
  = NoChildFeatures GroupId
  | ParentFeatureExists FeatureId
  | NoCycleFromGroup GroupId
  | GroupIsWellFormed GroupId
  deriving (Show, Eq, Read, Generic)
  deriving
    (FromJSON, ToJSON)
    via Prefixed "_" GroupDependencyType

------------------------------------------------------------------------
--                            CLI OPTIONS                             --
------------------------------------------------------------------------

data EvolutionPlanType
  = TreeUserType
  | FlatUserType
  | FlatModificationType
  deriving (Show, Eq, Read)

data Mode
  = GenerateOne String
  | GenerateAll
  | FromFile FilePath
  deriving (Show, Eq, Read)

data CliOptions = CliOptions
  { _mode :: Mode
  , _fromType :: EvolutionPlanType
  , _toType :: EvolutionPlanType
  , _print :: Bool
  , _generateElm :: Bool
  , _toFile :: Maybe FilePath
  }
  deriving (Show, Eq, Read)
