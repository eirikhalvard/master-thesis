module Merge.ChangeDetection where

import           Merge.MergeTypes
import           Types

import qualified Data.Map         as M
import qualified Data.Set         as S

-- Three levels of evolution plans.
--   - User level       - Represents the evolution plan in the way
--                        the user created the evolution plan,
--                        with lists of operations
--   - Abstracted level - Represents the evolution plan as a simple
--                        list of feature models. This representation
--                        is suitable with the granularity of the merge
--                        algorithm, and will be suitable as input to
--                        the merger
--   - Merge level      - Represents the feature model as an initial model
--                        and a SET of changes for each time point. No regard
--                        to how the changes should be implemented. Suitable
--                        for deriving the diff between the base and derived versions.
--

-- TODO: proper data structures for the input and output.
-- deriveChanges will transform the abstract level evolution plan to the merge
-- level evolution plan. This will be done by deriving what modifications were
-- done to each feature and group.  For every concecutive pair of feature
-- models, the changes between them will be derived
deriveChanges :: [(Int, FeatureModel)] -> (FeatureModel, [(Int, Modifications)])
deriveChanges evolutionPlan = undefined


-- diffFeatureModels will derive every modification
diffFeatureModels :: FeatureModel -> FeatureModel -> Modifications
diffFeatureModels fm1 fm2 = undefined
