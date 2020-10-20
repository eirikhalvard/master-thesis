module Merge.ChangeDetection where

import           Merge.MergeTypes
import           Types

import qualified Data.Map         as M
import qualified Data.Set         as S

deriveChanges :: [(Int, FeatureModel)] -> (FeatureModel, [(Int, Changes)])
deriveChanges evolutionPlan = undefined


diffFeatureModels :: FeatureModel -> FeatureModel -> Changes
diffFeatureModels fm1 fm2 = undefined
