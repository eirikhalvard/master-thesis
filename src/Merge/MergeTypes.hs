module Merge.MergeTypes where

import           Types

import qualified Data.Map as M
import qualified Data.Set as S


--- Evolution Plan Representations ---


-- Three levels of evolution plans.
--   - User level         - Represents the evolution plan in the way the user
--                          created the evolution plan, with lists of operations
--                          for each time point
--   - Abstracted level   - Represents the evolution plan as a simple list
--                          of feature models. This representation is suitable
--                          with the granularity of the merge algorithm, and
--                          will be suitable as input to the merger
--   - Modification level - Represents the feature model as an initial model
--                          and a SET of modifications for each time point. No
--                          regard to how and in what order the modifications
--                          should be implemented. Suitable for deriving the
--                          diff between the base and derived versions.
--   - Merge level        - Represents the feature model as an initial model and
--                          and the result from the diff for each time point.
--                          The diff will include the union of the changes from
--                          version 1 and version 2, which represents all the
--                          modifications that are to take place in the merged
--                          result
--

-- TODO: proper data structures for the abstracted and merge level evolution plans



type UserLevelEvolutionPlan = EvolutionPlan


type AbstractedLevelEvolutionPlan = [(Int, FeatureModel)]


type ModificationLevelEvolutionPlan = (FeatureModel, [(Int, Modifications)])


type MergeLevelEvolutionPlan = (FeatureModel, [(Int, DiffResult)])


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
