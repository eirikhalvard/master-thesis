module PlanMerging where

import           Types
import           Helpers

import           Control.Lens
import qualified Data.Map                      as M
import qualified Data.Set                      as S

performOperations :: EvolutionPlan -> [(Int, FeatureModel)]
performOperations (EvolutionPlan initTime initFM plans) =
  scanl applyOperations (initTime, initFM) plans


applyOperations
  :: (Int, FeatureModel) -> Plan -> (Int, FeatureModel)
applyOperations (_, currentFM) (Plan newTimePoint newOperations)
  = ( newTimePoint
    , foldl applyOperation currentFM newOperations
    )


applyOperation :: FeatureModel -> Operation -> FeatureModel
applyOperation currentFM op = case op of
  AddFeature    op' -> applyAddFeature op' currentFM
  RemoveFeature op' -> applyRemoveFeature op' currentFM
  MoveFeature   op' -> applyMoveFeature op' currentFM
  RenameFeature op' -> applyRenameFeature op' currentFM
  ChangeFeatureType op' ->
    applyChangeFeatureType op' currentFM
  AddGroup        op' -> applyAddGroup op' currentFM
  RemoveGroup     op' -> applyRemoveGroup op' currentFM
  ChangeGroupType op' -> applyChangeGroupType op' currentFM
  MoveGroup       op' -> applyMoveGroup op' currentFM

applyAddFeature
  :: AddFeatureOp -> FeatureModel -> FeatureModel
applyAddFeature (AddFeatureOp newFeatureId newName newParentGroupId newFeatureType) currentFM
  = currentFM
    & (features %~ M.insert newFeatureId newFeature)
    & (  groupOfGroupId newParentGroupId
      .  featureIds
      %~ S.insert newFeatureId
      )
  where
    newFeature = Feature newName
                         (Just newParentGroupId)
                         M.empty
                         newFeatureType

applyRemoveFeature
  :: RemoveFeatureOp -> FeatureModel -> FeatureModel
applyRemoveFeature op currentFM = undefined

applyMoveFeature
  :: MoveFeatureOp -> FeatureModel -> FeatureModel
applyMoveFeature op currentFM = undefined

applyRenameFeature
  :: RenameFeatureOp -> FeatureModel -> FeatureModel
applyRenameFeature op currentFM = undefined

applyChangeFeatureType
  :: ChangeFeatureTypeOp -> FeatureModel -> FeatureModel
applyChangeFeatureType op currentFM = undefined

applyAddGroup :: AddGroupOp -> FeatureModel -> FeatureModel
applyAddGroup op currentFM = undefined

applyRemoveGroup
  :: RemoveGroupOp -> FeatureModel -> FeatureModel
applyRemoveGroup op currentFM = undefined

applyChangeGroupType
  :: ChangeGroupTypeOp -> FeatureModel -> FeatureModel
applyChangeGroupType op currentFM = undefined

applyMoveGroup
  :: MoveGroupOp -> FeatureModel -> FeatureModel
applyMoveGroup op currentFM = undefined

