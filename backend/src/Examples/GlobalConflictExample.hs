module Examples.GlobalConflictExample where

import Control.Lens

import Examples.SoundExample
import qualified Lenses as L
import Types

-- Moving group to form cycle
-- v1 is changing the parent of an addGroup
-- while v2 is changing the parent of an existing group
-- each group is moved to a child feature of the other group
-- which forms a cycle
groupMoveCycle :: MergeInputData FlatModificationEvolutionPlan
groupMoveCycle =
  MergeInputData
    "Conflict, Global - Group Move Cycle"
    baseConstructedEvolutionPlan
    ( v1ConstructedEvolutionPlan
        & L.plans
          . traversed
          . filtered (has $ L.timePoint . only 3)
          . L.transformation
          . L.groups
          . ix "group:milk-group"
          %~ \(GroupAdd _ gType) -> GroupAdd "feature:regular" gType
    )
    ( v2ConstructedEvolutionPlan
        & L.plans
          . traversed
          . filtered (has $ L.timePoint . only 3)
          . L.transformation
          . L.groups
          . at "group:size-group"
          ?~ GroupModification (Just $ GroupParentModification "feature:normal") Nothing
    )
    ( Just $
        Left $
          Global
            3
            ( FailedDependencies
                [ GroupDependency
                    (GroupModification (Just $ GroupParentModification "feature:normal") Nothing)
                    (NoCycleFromGroup "group:size-group")
                ]
            )
    )

-- Moving group to form cycle
-- v1 is changing the parent of an addGroup
-- while v2 is changing the parent of an existing group
-- each group is moved to a child feature of the other group
-- which forms a cycle
violatingFeatureWellFormed :: MergeInputData FlatModificationEvolutionPlan
violatingFeatureWellFormed =
  MergeInputData
    "Conflict, Global - Well-formedness violation"
    baseConstructedEvolutionPlan
    v1ConstructedEvolutionPlan
    ( v2ConstructedEvolutionPlan
        & L.plans
          . traversed
          . filtered (has $ L.timePoint . only 3)
          . L.transformation
          . L.groups
          . at "group:vending-machine-group"
          ?~ GroupModification Nothing (Just $ GroupTypeModification Or)
        & L.plans
          . traversed
          . filtered (has $ L.timePoint . only 3)
          . L.transformation
          . L.features
          . at "feature:beverages"
          ?~ FeatureModification Nothing (Just $ FeatureTypeModification Optional) Nothing
    )
    ( Just $
        Left $
          Global
            3
            ( FailedDependencies
                [ GroupDependency
                    (GroupModification Nothing (Just $ GroupTypeModification Or))
                    (GroupIsWellFormed "group:size-group")
                ]
            )
    )

-- Adding a group to a non-existing feature
-- v1 is removing a feature in tp 1
-- while v2 is adding a group to the removed feature
missingParentFeature :: MergeInputData FlatModificationEvolutionPlan
missingParentFeature =
  MergeInputData
    "Conflict, Global - Missing Parent Feature"
    baseConstructedEvolutionPlan
    ( v1ConstructedEvolutionPlan
        & L.plans
          . traversed
          . filtered (has $ L.timePoint . only 2)
          . L.transformation
          . L.features
          . at "feature:coffee"
          ?~ FeatureRemove
    )
    ( v2ConstructedEvolutionPlan
        & L.plans
          . traversed
          . filtered (has $ L.timePoint . only 3)
          . L.transformation
          . L.groups
          . at "group:new-coffee-group"
          ?~ GroupAdd "feature:coffee" And
    )
    ( Just $
        Left $
          Global
            3
            ( FailedDependencies
                [ GroupDependency
                    (GroupAdd "feature:coffee" And)
                    (ParentFeatureExists "feature:coffee")
                ]
            )
    )
