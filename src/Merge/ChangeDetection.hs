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
flattenFeatureModel fm =
  FeatureModel'
    (fm ^. L.rootFeature . L.id)
    (M.fromList features)
    (M.fromList groups)
  where
    (features, groups) = flattenFeature Nothing (fm ^. L.rootFeature)
    flattenFeature mParentGroup (Feature id featureType name groups) =
      ([(id, Feature' mParentGroup featureType name)], [])
        <> foldMap (flattenGroup id) groups
    flattenGroup parentFeature (Group id groupType features) =
      ([], [(id, Group' parentFeature groupType)])
        <> foldMap (flattenFeature (Just id)) features

constructModificationLevelEP ::
  AbstractedLevelEvolutionPlan FeatureModel' ->
  ModificationLevelEvolutionPlan FeatureModel'
constructModificationLevelEP = undefined

-- diffFeatureModels will derive every modification
diffFeatureModels :: FeatureModel' -> FeatureModel' -> Modifications
diffFeatureModels fm1 fm2 = undefined
