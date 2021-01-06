module Merge.PlanMerging where

import qualified Lenses as L
import Merge.Types
import Types

import Control.Lens
import qualified Data.Map as M
import qualified Data.Map.Merge.Lazy as Merge

------------------------------------------------------------------------
--                         Create Merge Plan                          --
------------------------------------------------------------------------

createMergePlan ::
  ModificationLevelEvolutionPlan FeatureModel' ->
  ModificationLevelEvolutionPlan FeatureModel' ->
  ModificationLevelEvolutionPlan FeatureModel' ->
  MergeLevelEvolutionPlan FeatureModel'
createMergePlan base v1 v2 =
  base & L.plans
    %~ \basePlans -> mergePlans basePlans (v1 ^. L.plans) (v2 ^. L.plans)

mergePlans ::
  [Plan Modifications] ->
  [Plan Modifications] ->
  [Plan Modifications] ->
  [Plan DiffResult]
mergePlans basePlans v1Plans v2Plans =
  mergePlansWithTimes
    (collectAllTimePoints basePlans v1Plans v2Plans)
    basePlans
    v1Plans
    v2Plans

mergePlansWithTimes ::
  [Time] ->
  [Plan Modifications] ->
  [Plan Modifications] ->
  [Plan Modifications] ->
  [Plan DiffResult]
mergePlansWithTimes [] _ _ _ = []
mergePlansWithTimes (time : times) basePlans v1Plans v2Plans =
  Plan time (diffModifications baseModifications v1Modifications v2Modifications) :
  mergePlansWithTimes
    times
    nextBasePlans
    nextV1Plans
    nextV2Plans
  where
    (baseModifications, nextBasePlans) = getModificationForTime basePlans time
    (v1Modifications, nextV1Plans) = getModificationForTime v1Plans time
    (v2Modifications, nextV2Plans) = getModificationForTime v2Plans time

collectAllTimePoints :: [Plan a] -> [Plan a] -> [Plan a] -> [Time]
collectAllTimePoints basePlans v1Plans v2Plans =
  merge (merge baseTimes v1Times) v2Times
  where
    baseTimes = basePlans ^.. traversed . L.timePoint
    v1Times = v1Plans ^.. traversed . L.timePoint
    v2Times = v2Plans ^.. traversed . L.timePoint
    merge (x : xs) (y : ys)
      | x == y = x : merge xs ys
      | x < y = x : merge xs (y : ys)
      | otherwise = y : merge (x : xs) ys
    merge xs ys = xs ++ ys

getModificationForTime :: [Plan Modifications] -> Time -> (Modifications, [Plan Modifications])
getModificationForTime [] _ = (emptyModifications, [])
getModificationForTime plans@(Plan planTime modification : rest) time =
  if time == planTime
    then (modification, rest)
    else (emptyModifications, plans)

emptyModifications :: Modifications
emptyModifications = Modifications M.empty M.empty

-- diffModifications will compare the modifcations from base with the
-- modifications from each derived version. The comparison will produce
-- a DiffResult that represents how every feature- and group modification was
-- changed between the base and derived versions
diffModifications :: Modifications -> Modifications -> Modifications -> DiffResult
diffModifications base v1 v2 =
  DiffResult
    (mergeMaps (base ^. L.features) (v1 ^. L.features) (v2 ^. L.features))
    (mergeMaps (base ^. L.groups) (v1 ^. L.groups) (v2 ^. L.groups))
  where
    mergeMaps baseMap v1Map v2Map =
      mergeBaseAndDerived
        baseMap
        $ mergeDerived v1Map v2Map

mergeBaseAndDerived ::
  (Ord a, Eq modification) =>
  M.Map a modification ->
  M.Map a (DerivedComparisionResult modification) ->
  M.Map a (SingleDiffResult modification)
mergeBaseAndDerived =
  Merge.merge
    ( Merge.mapMissing
        (\_ baseMod -> withBase baseMod Nothing Nothing)
    )
    ( Merge.mapMissing
        ( \_ derivedResult -> case derivedResult of
            OneVersion version mod ->
              ChangedInOne
                version
                (OneChangeWithoutBase (AddedModification mod))
            BothVersions v1Mod v2Mod ->
              ChangedInBoth
                ( BothChangeWithoutBase
                    (AddedModification v1Mod)
                    (AddedModification v2Mod)
                )
        )
    )
    ( Merge.zipWithMatched
        ( \_ baseMod derivedResult ->
            case derivedResult of
              OneVersion V1 mod ->
                withBase baseMod (Just mod) Nothing
              OneVersion V2 mod ->
                withBase baseMod Nothing (Just mod)
              BothVersions v1Mod v2Mod ->
                withBase baseMod (Just v1Mod) (Just v2Mod)
        )
    )
  where
    withBase baseMod mV1Mod mV2Mod =
      case (Just baseMod /= mV1Mod, Just baseMod /= mV2Mod) of
        (True, True) ->
          ChangedInBoth
            ( BothChangeWithBase
                baseMod
                (removeOrChanged mV1Mod)
                (removeOrChanged mV2Mod)
            )
        (True, False) ->
          ChangedInOne
            V1
            ( OneChangeWithBase
                baseMod
                (removeOrChanged mV1Mod)
            )
        (False, True) ->
          ChangedInOne
            V2
            ( OneChangeWithBase
                baseMod
                (removeOrChanged mV2Mod)
            )
        (False, False) -> NoChange baseMod
    removeOrChanged Nothing = RemovedModification
    removeOrChanged (Just mod) = ChangedModification mod

data DerivedComparisionResult modification
  = OneVersion Version modification
  | BothVersions modification modification

mergeDerived ::
  Ord a =>
  M.Map a modification ->
  M.Map a modification ->
  M.Map a (DerivedComparisionResult modification)
mergeDerived =
  Merge.merge
    (Merge.mapMissing (const (OneVersion V1)))
    (Merge.mapMissing (const (OneVersion V2)))
    (Merge.zipWithMatched (const BothVersions))

------------------------------------------------------------------------
--                          Unify Merge Plan                          --
------------------------------------------------------------------------

unifyMergePlan ::
  MergeLevelEvolutionPlan FeatureModel' ->
  Either Conflict (ModificationLevelEvolutionPlan FeatureModel')
unifyMergePlan =
  L.plans . traversed %%~ unifyTimePointResult

unifyTimePointResult ::
  Plan DiffResult ->
  Either Conflict (Plan Modifications)
unifyTimePointResult (Plan timePoint (DiffResult features groups)) = do
  features' <- unifyModificationsMap checkOverlappingFeatureChanges timePoint features
  groups' <- unifyModificationsMap checkOverlappingGroupChanges timePoint groups
  return $ Plan timePoint (Modifications features' groups')

unifyModificationsMap ::
  (BothChange modificationType -> Either Conflict (Maybe modificationType)) ->
  Time ->
  M.Map modificationIdType (SingleDiffResult modificationType) ->
  Either Conflict (M.Map modificationIdType modificationType)
unifyModificationsMap checkBothOverlapping timePoint =
  M.traverseMaybeWithKey (const $ unifySingleDiffResult checkBothOverlapping timePoint)

unifySingleDiffResult ::
  (BothChange modificationType -> Either Conflict (Maybe modificationType)) ->
  Time ->
  SingleDiffResult modificationType ->
  Either Conflict (Maybe modificationType)
unifySingleDiffResult checkBothOverlapping timePoint singleDiffResult =
  case singleDiffResult of
    NoChange baseModification ->
      Right (Just baseModification)
    ChangedInOne version (OneChangeWithBase baseModification RemovedModification) ->
      Right Nothing
    ChangedInOne version (OneChangeWithBase baseModification (ChangedModification derivedModification)) ->
      Right (Just derivedModification)
    ChangedInOne version (OneChangeWithoutBase (AddedModification derivedModification)) ->
      Right (Just derivedModification)
    ChangedInBoth bothChange ->
      checkBothOverlapping bothChange

checkOverlappingFeatureChanges ::
  BothChange FeatureModification ->
  Either Conflict (Maybe FeatureModification)
checkOverlappingFeatureChanges =
  checkOverlappingChanges
    ConflictingFeatureModificationWithBase
    ConflictingFeatureModificationWithoutBase

checkOverlappingGroupChanges ::
  BothChange GroupModification ->
  Either Conflict (Maybe GroupModification)
checkOverlappingGroupChanges =
  checkOverlappingChanges
    ConflictingGroupModificationWithBase
    ConflictingGroupModificationWithoutBase

checkOverlappingChanges ::
  (mod -> mod -> mod -> MergeConflict) ->
  (mod -> mod -> MergeConflict) ->
  BothChange mod ->
  Either Conflict (Maybe mod)
checkOverlappingChanges conflictWithBase conflictWithoutBase bothChange =
  case bothChange of
    BothChangeWithBase baseModification v1 v2 -> undefined
    BothChangeWithoutBase (AddedModification v1) (AddedModification v2) ->
      undefined

-- unifySingleDiffResult timePoint (NoChange modificationType) = Right (Just modificationType)
-- unifySingleDiffResult timePoint (ChangedInOne version oneChange) =
--   Left (Panic timePoint "not implemented")
-- unifySingleDiffResult timePoint (ChangedInBoth bothChange) =
--   Left (Panic timePoint "not implemented")
