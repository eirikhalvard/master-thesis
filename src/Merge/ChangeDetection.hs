module Merge.ChangeDetection where

import           Merge.MergeTypes
import           Types

import qualified Data.Map         as M
import qualified Data.Set         as S

-- deriveChanges will transform the abstract level evolution plan to the merge
-- level evolution plan. This will be done by deriving what modifications were
-- done to each feature and group.  For every concecutive pair of feature
-- models, the changes between them will be derived
deriveChanges :: AbstractedLevelEvolutionPlan -> ModificationLevelEvolutionPlan
deriveChanges evolutionPlan = undefined


-- diffFeatureModels will derive every modification
diffFeatureModels :: FeatureModel -> FeatureModel -> Modifications
diffFeatureModels fm1 fm2 = undefined
