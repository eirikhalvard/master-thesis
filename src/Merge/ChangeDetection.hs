module Merge.ChangeDetection where

import qualified Lenses as L
import Types

import Control.Lens
import qualified Data.Map as M
import qualified Data.Set as S

-- deriveChanges will transform the abstract level evolution plan to the
-- modification level evolution plan. This will be done by deriving what
-- modifications were done to each feature and group.  For every concecutive
-- pair of feature models, the changes between them will be derived

deriveChanges ::
  AbstractedLevelEvolutionPlan FeatureModel ->
  ModificationLevelEvolutionPlan FeatureModel'
deriveChanges = constructModificationLevelEP . flattenEvolutionPlan

flattenEvolutionPlan ::
  AbstractedLevelEvolutionPlan FeatureModel ->
  AbstractedLevelEvolutionPlan FeatureModel'
flattenEvolutionPlan =
  L.timePoints
    . traversed
    . L.featureModel
    %~ flattenFeatureModel

flattenFeatureModel :: FeatureModel -> FeatureModel'
flattenFeatureModel = undefined
  where
    flattenFeature feature mParentGroup = undefined
    flattenGroup group parentFeature = undefined

constructModificationLevelEP ::
  AbstractedLevelEvolutionPlan FeatureModel' ->
  ModificationLevelEvolutionPlan FeatureModel'
constructModificationLevelEP = undefined

-- diffFeatureModels will derive every modification
diffFeatureModels :: FeatureModel' -> FeatureModel' -> Modifications
diffFeatureModels fm1 fm2 = undefined
