module Merge.PlanMerging where

import qualified Lenses as L
import Types

import Control.Lens
import qualified Data.Map as M
import qualified Data.Map.Merge.Lazy as Merge

------------------------------------------------------------------------
--                         Create Merge Plan                          --
------------------------------------------------------------------------

createMergePlan ::
  FlatModificationEvolutionPlan ->
  FlatModificationEvolutionPlan ->
  FlatModificationEvolutionPlan ->
  MergeEvolutionPlan FlatFeatureModel
createMergePlan base v1 v2 =
  base & L.plans
    %~ \basePlans ->
      mergePlans
        basePlans
        (v1 ^. L.plans)
        (v2 ^. L.plans)

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
  Plan
    time
    ( diffModifications
        baseModifications
        v1Modifications
        v2Modifications
    ) :
  mergePlansWithTimes
    times
    nextBasePlans
    nextV1Plans
    nextV2Plans
  where
    (baseModifications, nextBasePlans) =
      getModificationForTime basePlans time
    (v1Modifications, nextV1Plans) =
      getModificationForTime v1Plans time
    (v2Modifications, nextV2Plans) =
      getModificationForTime v2Plans time

collectAllTimePoints ::
  [Plan a] ->
  [Plan a] ->
  [Plan a] ->
  [Time]
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

getModificationForTime ::
  [Plan Modifications] ->
  Time ->
  (Modifications, [Plan Modifications])
getModificationForTime [] _ = (emptyModifications, [])
getModificationForTime plans time =
  let Plan planTime modification : rest = plans
   in if time == planTime
        then (modification, rest)
        else (emptyModifications, plans)

emptyModifications :: Modifications
emptyModifications = Modifications M.empty M.empty

-- diffModifications will compare the modifcations from base with the
-- modifications from each derived version. The comparison will produce
-- a DiffResult that represents how every feature- and group modification was
-- changed between the base and derived versions
diffModifications ::
  Modifications ->
  Modifications ->
  Modifications ->
  DiffResult
diffModifications base v1 v2 =
  DiffResult
    ( mergeMaps
        (base ^. L.features)
        (v1 ^. L.features)
        (v2 ^. L.features)
    )
    ( mergeMaps
        (base ^. L.groups)
        (v1 ^. L.groups)
        (v2 ^. L.groups)
    )
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
    (Merge.mapMissing (const inBase))
    (Merge.mapMissing (const inDerived))
    (Merge.zipWithMatched (const inBoth))
  where
    inBase baseMod = withBase baseMod Nothing Nothing
    inDerived derivedResult =
      case derivedResult of
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
    inBoth baseMod derivedResult =
      case derivedResult of
        OneVersion V1 mod ->
          withBase baseMod (Just mod) Nothing
        OneVersion V2 mod ->
          withBase baseMod Nothing (Just mod)
        BothVersions v1Mod v2Mod ->
          withBase baseMod (Just v1Mod) (Just v2Mod)
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
  MergeEvolutionPlan FlatFeatureModel ->
  Either Conflict FlatModificationEvolutionPlan
unifyMergePlan =
  L.plans . traversed %%~ unifyTimePointResult

unifyTimePointResult ::
  Plan DiffResult ->
  Either Conflict (Plan Modifications)
unifyTimePointResult (Plan timePoint (DiffResult features groups)) = do
  features' <- unifyModificationsMap FeatureConflict timePoint features
  groups' <- unifyModificationsMap GroupConflict timePoint groups
  return $ Plan timePoint (Modifications features' groups')

unifyModificationsMap ::
  Eq modificationType =>
  (modificationIdType -> BothChange modificationType -> MergeConflict) ->
  Time ->
  M.Map modificationIdType (SingleDiffResult modificationType) ->
  Either Conflict (M.Map modificationIdType modificationType)
unifyModificationsMap checkBothOverlapping timePoint =
  M.traverseMaybeWithKey
    (unifySingleDiffResult checkBothOverlapping timePoint)

unifySingleDiffResult ::
  Eq modificationType =>
  (modificationIdType -> BothChange modificationType -> MergeConflict) ->
  Time ->
  modificationIdType ->
  SingleDiffResult modificationType ->
  Either Conflict (Maybe modificationType)
unifySingleDiffResult overlappingToMergeConflict timePoint id singleDiffResult =
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
      checkOverlappingChanges
        overlappingToMergeConflict
        timePoint
        id
        bothChange

checkOverlappingChanges ::
  Eq modificationType =>
  (modificationIdType -> BothChange modificationType -> MergeConflict) ->
  Time ->
  modificationIdType ->
  BothChange modificationType ->
  Either Conflict (Maybe modificationType)
checkOverlappingChanges overlappingToMergeConflict timePoint id bothChange =
  case bothChange of
    BothChangeWithoutBase (AddedModification v1) (AddedModification v2) ->
      ensureNotConflicting v1 v2
    BothChangeWithBase base RemovedModification RemovedModification ->
      Right Nothing
    BothChangeWithBase base (ChangedModification v1) (ChangedModification v2) ->
      ensureNotConflicting v1 v2
    BothChangeWithBase{} ->
      conflict
  where
    conflict = Left (Merge timePoint (overlappingToMergeConflict id bothChange))
    ensureNotConflicting v1Modification v2Modification =
      if v1Modification == v2Modification
        then Right (Just v1Modification)
        else conflict
