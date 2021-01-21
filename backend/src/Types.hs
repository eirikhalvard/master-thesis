{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Types where

import Control.Lens
import qualified Data.Map as M
import qualified Data.Set as S

import Data.Aeson
import GHC.Generics

----------------------
--  FEATURE MODELS  --
----------------------

type FeatureId = String

type GroupId = String

--- Tree Structured Feature Model ---

data TreeFeatureModel = TreeFeatureModel
  { _rootFeature :: TreeFeature
  }
  deriving (Show, Eq, Read, Generic)

data TreeFeature = TreeFeature
  { _id :: FeatureId
  , _featureType :: FeatureType
  , _name :: String
  , _groups :: S.Set TreeGroup
  }
  deriving (Show, Eq, Read, Ord, Generic)

data TreeGroup = TreeGroup
  { _id :: GroupId
  , _groupType :: GroupType
  , _features :: S.Set TreeFeature
  }
  deriving (Show, Eq, Read, Ord, Generic)

--- Flat Structured Feature Model ---

data FlatFeatureModel = FlatFeatureModel
  { _rootId :: FeatureId
  , _features :: M.Map FeatureId FlatFeature
  , _groups :: M.Map GroupId FlatGroup
  }
  deriving (Show, Eq, Read)

data FlatFeature = FlatFeature
  { _parentGroupId :: Maybe GroupId
  , _featureType :: FeatureType
  , _name :: String
  }
  deriving (Show, Eq, Read)

data FlatGroup = FlatGroup
  { _parentFeatureId :: FeatureId
  , _groupType :: GroupType
  }
  deriving (Show, Eq, Read)

data FeatureType
  = Optional
  | Mandatory
  deriving (Show, Eq, Read, Ord, Generic)

data GroupType
  = And
  | Or
  | Alternative
  deriving (Show, Eq, Read, Ord, Generic)

-----------------------
--  EVOLUTION PLANS  --
-----------------------

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
--      Modification Level Transformation:
--        Represents the transformation as a set of modifications. This
--        representation guarantees that each there are no conflicting
--        modifications, i.e. moving a feature twice. This allows for merging
--        the modifications in an arbitrary ordering, since no modifications
--        shadow others, etc.
--      Merge Level Transformation:
--        The merge level transformation represents the "planned"
--        transformations from both versions in the merge. The transformation
--        is essentially the union of the modifications of version 1 and
--        version 2. In this representation, a feature might be planned to be
--        changed, added or removed in several versions, which this
--        representation encodes.

type UserTree = UserLevelEvolutionPlan TreeFeatureModel

type UserFlat = UserLevelEvolutionPlan FlatFeatureModel

type ModFlat = ModificationLevelEvolutionPlan FlatFeatureModel

type Time = Int

data UserLevelEvolutionPlan featureModel = UserLevelEvolutionPlan
  { _timePoints :: [TimePoint featureModel]
  }
  deriving (Show, Eq, Read, Generic)

data TimePoint featureModel = TimePoint
  { _time :: Time
  , _featureModel :: featureModel
  }
  deriving (Show, Eq, Read, Generic)

data TransformationEvolutionPlan transformation featureModel = TransformationEvolutionPlan
  { _initialTime :: Time
  , _initialFM :: featureModel
  , _plans :: [Plan transformation]
  }
  deriving (Show, Eq, Read)

data Plan transformation = Plan
  { _timePoint :: Time
  , _transformation :: transformation
  }
  deriving (Show, Eq, Read)

type ModificationLevelEvolutionPlan featureModel = TransformationEvolutionPlan Modifications featureModel

type MergeLevelEvolutionPlan featureModel = TransformationEvolutionPlan DiffResult featureModel

----------------------------
--  TRANSFORMATION TYPES  --
----------------------------

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
  deriving (Show, Eq, Read)

data FeatureModification
  = FeatureAdd GroupId FeatureType String
  | FeatureRemove
  | FeatureModification
      (Maybe FeatureParentModification)
      (Maybe FeatureTypeModification)
      (Maybe FeatureNameModification)
  deriving (Show, Eq, Read)

data FeatureParentModification
  = FeatureParentModification GroupId
  deriving (Show, Eq, Read)

data FeatureNameModification
  = FeatureNameModification String
  deriving (Show, Eq, Read)

data FeatureTypeModification
  = FeatureTypeModification FeatureType
  deriving (Show, Eq, Read)

data GroupModification
  = GroupAdd FeatureId GroupType
  | GroupRemove
  | GroupModification
      (Maybe GroupParentModification)
      (Maybe GroupTypeModification)
  deriving (Show, Eq, Read)

data GroupParentModification
  = GroupParentModification FeatureId
  deriving (Show, Eq, Read)

data GroupTypeModification
  = GroupTypeModification GroupType
  deriving (Show, Eq, Read)

--- DIFF RESULT ---

-- The diff result from the all the changes in the entire time point for all
-- versions of the model
data DiffResult = DiffResult
  { _features :: M.Map FeatureId FeatureDiffResult
  , _groups :: M.Map GroupId GroupDiffResult
  }
  deriving (Show, Eq, Read)

type FeatureDiffResult =
  SingleDiffResult FeatureModification

type GroupDiffResult =
  SingleDiffResult GroupModification

-- Every possible combination that a feature- or group change could be modified
data SingleDiffResult modificationType
  = NoChange modificationType
  | ChangedInOne Version (OneChange modificationType)
  | ChangedInBoth (BothChange modificationType)
  deriving (Show, Eq, Read)

data OneChange modificationType
  = OneChangeWithBase
      modificationType -- Base modification
      (RemovedOrChangedModification modificationType) -- Derived (V1 or V2) modification
  | OneChangeWithoutBase
      (AddedModification modificationType) -- Derived (V1 or V2) modification
  deriving (Show, Eq, Read)

data BothChange modificationType
  = BothChangeWithBase
      modificationType -- Base modification
      (RemovedOrChangedModification modificationType) -- V1 modification
      (RemovedOrChangedModification modificationType) -- V2 modification
  | BothChangeWithoutBase
      (AddedModification modificationType) -- V1 modification
      (AddedModification modificationType) -- V2 modification
  deriving (Show, Eq, Read)

data RemovedOrChangedModification modificationType
  = RemovedModification
  | ChangedModification modificationType
  deriving (Show, Eq, Read)

data AddedModification modificationType
  = AddedModification modificationType
  deriving (Show, Eq, Read)

data Version
  = V1
  | V2
  deriving (Show, Eq, Read)

------------------------------------------------------------------------
--                           Merge Artifact                           --
------------------------------------------------------------------------

data MergeArtifact evolutionPlan = MergeArtifact
  { _name :: String
  , _base :: evolutionPlan
  , _v1 :: evolutionPlan
  , _v2 :: evolutionPlan
  }
  deriving (Show, Eq, Read, Generic, Functor)
