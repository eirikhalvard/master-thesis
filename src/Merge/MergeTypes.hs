module Merge.MergeTypes where

import           Types

import qualified Data.Map as M
import qualified Data.Set as S


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
  { featureModifications :: M.Map FeatureId FeatureModification
  , groupModifications   :: M.Map GroupId GroupModification
  }


data FeatureModification
  = FeatureAdd
  | FeatureRemove
  | FeatureModification
      (Maybe FeatureParentModification)
      (Maybe FeatureNameModification)
      (Maybe FeatureTypeModification)


data FeatureParentModification = FeatureParentModification GroupId


data FeatureNameModification = FeatureNameModification String


data FeatureTypeModification = FeatureTypeModification FeatureType


data GroupModification
  = GroupAdd
  | GroupRemove
  | GroupModification
      (Maybe GroupParentModification)
      (Maybe GroupTypeModification)


data GroupParentModification = GroupParentModification FeatureId


data GroupTypeModification = GroupTypeModification GroupType



--- Diff between base, v1, v2 ---

-- The diff result from the all the changes in the entire time point for all
-- versions of the model
data DiffResult = DiffResult
  { featureDiffResult :: M.Map FeatureId FeatureDiffResult
  , groupDiffResult   :: M.Map GroupId GroupDiffResult
  }


-- Every possible combination that a feature- or group change could be modified
data SingleDiffResult modificationType
  = NoChange modificationType
  | ChangedInOne Version (OneChange modificationType)
  | ChangedInBoth (BothChange modificationType)


data OneChange modificationType
  = OneChangeWithBase
      modificationType -- Base modification
      (RemovedOrChangedModification modificationType) -- Derived (V1 or V2) modification
  | OneChangeWithoutBase
      (AddedModification modificationType) -- Derived (V1 or V2) modification


data BothChange modificationType
  = BothChangeWithBase
      modificationType -- Base modification
      (RemovedOrChangedModification modificationType) -- V1 modification
      (RemovedOrChangedModification modificationType) -- V2 modification
  | BothChangeWithoutBase
      (AddedModification modificationType) -- V1 modification
      (AddedModification modificationType) -- V2 modification


data RemovedOrChangedModification modificationType
  = RemovedModification
  | ChangedModification modificationType


data AddedModification modificationType
  = AddedModification modificationType


data Version
  = V1
  | V2


type FeatureDiffResult = SingleDiffResult FeatureModification


type GroupDiffResult = SingleDiffResult GroupModification
