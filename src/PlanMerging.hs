module PlanMerging where

import           Helpers
import           Types

import           Control.Lens
import qualified Data.Map     as M
import qualified Data.Set     as S

------------------------------------------------------------------------
--                         PERFORM OPERATIONS                         --
------------------------------------------------------------------------


performOperations :: EvolutionPlan -> [(Int, FeatureModel)]
performOperations (EvolutionPlan initTime initFM plans) =
  scanl applyOperations (initTime, initFM) plans


applyOperations :: (Int, FeatureModel) -> Plan -> (Int, FeatureModel)
applyOperations (_, currentFM) (Plan newTimePoint newOperations) =
  ( newTimePoint
  , foldl applyOperation currentFM newOperations
  )


applyOperation :: FeatureModel -> Operation -> FeatureModel
applyOperation currentFM op = case op of
  AddFeature op'        -> applyAddFeature op' currentFM
  RemoveFeature op'     -> applyRemoveFeature op' currentFM
  MoveFeature op'       -> applyMoveFeature op' currentFM
  RenameFeature op'     -> applyRenameFeature op' currentFM
  ChangeFeatureType op' -> applyChangeFeatureType op' currentFM
  AddGroup op'          -> applyAddGroup op' currentFM
  RemoveGroup op'       -> applyRemoveGroup op' currentFM
  ChangeGroupType op'   -> applyChangeGroupType op' currentFM
  MoveGroup op'         -> applyMoveGroup op' currentFM

applyAddFeature :: AddFeatureOp -> FeatureModel -> FeatureModel
applyAddFeature (AddFeatureOp newFeatureId newName newParentGroupId newFeatureType)
  = updateParent . addMapping
  where
    addMapping =
      features %~ M.insert newFeatureId newFeature
    updateParent =
      groupOfGroupId newParentGroupId
        .  featureIds
        %~ S.insert newFeatureId
    newFeature = Feature newName
                         (Just newParentGroupId)
                         M.empty
                         newFeatureType


applyRemoveFeature :: RemoveFeatureOp -> FeatureModel -> FeatureModel
applyRemoveFeature (RemoveFeatureOp removeFeatureId) =
  updateParent . removeMapping
  where
    removeMapping = features %~ M.delete removeFeatureId
    updateParent =
      parentGroupOfFeature removeFeatureId
        .  featureIds
        %~ S.delete removeFeatureId

applyMoveFeature :: MoveFeatureOp -> FeatureModel -> FeatureModel
applyMoveFeature (MoveFeatureOp moveFeatureId newGroupId) =
  updateNewParent . updateFeature . updateOldParent
  where
    updateOldParent =
      parentGroupOfFeature moveFeatureId
        .  featureIds
        %~ S.delete moveFeatureId
    updateFeature =
      features
        .  ix moveFeatureId
        .  parentGroupId
        .~ Just newGroupId
    updateNewParent =
      groupOfGroupId newGroupId
        .  featureIds
        %~ S.insert moveFeatureId

applyRenameFeature :: RenameFeatureOp -> FeatureModel -> FeatureModel
applyRenameFeature (RenameFeatureOp renameFeatureId newName)
  = renameFeature
  where
    renameFeature =
      features . ix renameFeatureId . name .~ newName

applyChangeFeatureType :: ChangeFeatureTypeOp -> FeatureModel -> FeatureModel
applyChangeFeatureType (ChangeFeatureTypeOp changeFeatureId newType)
  = changeType
  where
    changeType =
      features . ix changeFeatureId . featureType .~ newType

applyAddGroup :: AddGroupOp -> FeatureModel -> FeatureModel
applyAddGroup (AddGroupOp newGroupId parentId gType) =
  addGroup
  where
    addGroup =
      features
        .  ix parentId
        .  groups
        %~ M.insert newGroupId newGroup
    newGroup = Group gType S.empty

applyRemoveGroup :: RemoveGroupOp -> FeatureModel -> FeatureModel
applyRemoveGroup (RemoveGroupOp removeGroupId) =
  removeGroup
  where
    removeGroup =
      parentOfGroup removeGroupId
        .  groups
        %~ M.delete removeGroupId

applyChangeGroupType :: ChangeGroupTypeOp -> FeatureModel -> FeatureModel
applyChangeGroupType (ChangeGroupTypeOp changeGroupId newType)
  = changeGroupType
  where
    changeGroupType =
      groupOfGroupId changeGroupId . groupType .~ newType

applyMoveGroup :: MoveGroupOp -> FeatureModel -> FeatureModel
applyMoveGroup (MoveGroupOp moveGroupId newParentId) =
  removeAndAddGroup
  where
    removeAndAddGroup fm =
      let moveGroup = fm ^? groupOfGroupId moveGroupId
          removed =
              fm
                &  features
                .  traversed
                .  groups
                %~ M.delete moveGroupId
          addedAndRemoved =
              removed
                &  features
                .  ix newParentId
                .  groups
                .  at moveGroupId
                .~ moveGroup
      in  addedAndRemoved


------------------------------------------------------------------------
--                        DIFF EVOLUTION PLANS                        --
------------------------------------------------------------------------


type EvolutionPlanDiff = [PlanDiff]


type PlanDiff = (Int, [Change])


data Change = Change ChangeType Operation
  deriving ( Show, Eq )


data ChangeType
  = AddChange
  | RemoveChange
  deriving ( Show, Eq )


diffEvolutionPlans :: EvolutionPlan -> EvolutionPlan -> EvolutionPlanDiff
diffEvolutionPlans (EvolutionPlan baseTime baseModel basePlans) (EvolutionPlan derivedTime derivedModel derivedPlans)
  = initialPlanDiff : operationsDiff
  where
    initialPlanDiff =
      (baseTime, diffInitialModel baseModel derivedModel)
    operationsDiff = diffOperations basePlans derivedPlans


 -- Assumption: InitialModel are equal, and start at the same time.
diffInitialModel :: FeatureModel -> FeatureModel -> [Change]
diffInitialModel baseFM derivedFM = []


diffOperations :: [Plan] -> [Plan] -> EvolutionPlanDiff
diffOperations basePlans derivedPlans = []

