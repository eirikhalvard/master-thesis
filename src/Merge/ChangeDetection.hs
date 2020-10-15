module Merge.ChangeDetection where

import           Types

import qualified Data.Map as M
import qualified Data.Set as S

data Changes = Changes
  { featureChanges :: M.Map FeatureId FeatureChange
  , groupChanges   :: M.Map GroupId GroupChange
  }

data FeatureChange
  = FeatureAdd
  | FeatureRemove
  | FeatureChange
      (Maybe FeatureParentChange)
      (Maybe FeatureNameChange)
      (Maybe FeatureTypeChange)

data FeatureParentChange = FeatureParentChange GroupId

data FeatureNameChange = FeatureNameChange String

data FeatureTypeChange = FeatureTypeChange FeatureType

data GroupChange
  = GroupAdd
  | GroupRemove
  | GroupChange
      (Maybe GroupParentChange)
      (Maybe GroupTypeChange)

data GroupParentChange = GroupParentChange FeatureId

data GroupTypeChange = GroupTypeChange GroupType


deriveChanges :: [(Int, FeatureModel)] -> (FeatureModel, [(Int, Changes)])
deriveChanges evolutionPlan = undefined


diffFeatureModels :: FeatureModel -> FeatureModel -> Changes
diffFeatureModels fm1 fm2 = undefined
