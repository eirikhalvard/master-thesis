module Examples.LocalConflictExample where

import Control.Lens

import Examples.SoundExample
import qualified Lenses as L
import Types

-- Moving the addition of a feature
-- base and v1 is (still adding) a feature at tp 1
-- but v2 has moved the addition to tp 2
-- this will NOT result in an error!
movedFeatureAddition :: MergeInputData FlatModificationEvolutionPlan
movedFeatureAddition =
  MergeInputData
    "Sound - Moved Addition"
    baseConstructedEvolutionPlan
    v1ConstructedEvolutionPlan
    ( v2ConstructedEvolutionPlan
        & L.plans
          . traversed
          . filtered (has $ L.timePoint . only 1)
          . L.transformation
          . L.features
          . at "feature:coffee"
          .~ Nothing
        & L.plans
          . traversed
          . filtered (has $ L.timePoint . only 2)
          . L.transformation
          . L.features
          . at "feature:coffee"
          ?~ FeatureAdd "group:beverages-group" Optional "Coffee"
    )
    ( Just $
        Right $
          expectedConstructedEvolutionPlan
            & L.plans
              . traversed
              . filtered (has $ L.timePoint . only 1)
              . L.transformation
              . L.features
              . at "feature:coffee"
              .~ Nothing
            & L.plans
              . traversed
              . filtered (has $ L.timePoint . only 2)
              . L.transformation
              . L.features
              . at "feature:coffee"
              ?~ FeatureAdd "group:beverages-group" Optional "Coffee"
    )

-- Moving the addition of a feature
-- base is additing the feature at tp 1
-- but v1 and v2 has moved the addition to tp 2 and 3 respectively
conflictingAdditionMove :: MergeInputData FlatModificationEvolutionPlan
conflictingAdditionMove =
  MergeInputData
    "Conflict, Local - Conflicting Addition Move"
    baseConstructedEvolutionPlan
    ( v1ConstructedEvolutionPlan
        & L.plans
          . traversed
          . filtered (has $ L.timePoint . only 1)
          . L.transformation
          . L.features
          . at "feature:coffee"
          .~ Nothing
        & L.plans
          . traversed
          . filtered (has $ L.timePoint . only 2)
          . L.transformation
          . L.features
          . at "feature:coffee"
          ?~ FeatureAdd "group:beverages-group" Optional "Coffee"
    )
    ( v2ConstructedEvolutionPlan
        & L.plans
          . traversed
          . filtered (has $ L.timePoint . only 1)
          . L.transformation
          . L.features
          . at "feature:coffee"
          .~ Nothing
        & L.plans
          . traversed
          . filtered (has $ L.timePoint . only 3)
          . L.transformation
          . L.features
          . at "feature:coffee"
          ?~ FeatureAdd "group:beverages-group" Optional "Coffee"
    )
    ( Just $
        Left $
          Local
            3
            ( FeatureAlreadyExists
                (FeatureAdd "group:beverages-group" Optional "Coffee")
                "feature:coffee"
            )
    )

-- Removing a group at different times
-- adding a ghost group to the base at tp 1
-- and removing the group at tp 2 and 3 in the derived versions
conflictingGroupRemove :: MergeInputData FlatModificationEvolutionPlan
conflictingGroupRemove =
  MergeInputData
    "Conflict, Local - Conflicting Group Removal"
    ( baseConstructedEvolutionPlan
        & L.plans
          . traversed
          . filtered (has $ L.timePoint . only 1)
          . L.transformation
          . L.groups
          . at "group:ghost-group"
          ?~ GroupAdd "feature:vending-machine" Alternative
    )
    ( v1ConstructedEvolutionPlan
        & L.plans
          . traversed
          . filtered (has $ L.timePoint . only 1)
          . L.transformation
          . L.groups
          . at "group:ghost-group"
          ?~ GroupAdd "feature:vending-machine" Alternative
        & L.plans
          . traversed
          . filtered (has $ L.timePoint . only 2)
          . L.transformation
          . L.groups
          . at "group:ghost-group"
          ?~ GroupRemove
    )
    ( v2ConstructedEvolutionPlan
        & L.plans
          . traversed
          . filtered (has $ L.timePoint . only 1)
          . L.transformation
          . L.groups
          . at "group:ghost-group"
          ?~ GroupAdd "feature:vending-machine" Alternative
        & L.plans
          . traversed
          . filtered (has $ L.timePoint . only 3)
          . L.transformation
          . L.groups
          . at "group:ghost-group"
          ?~ GroupRemove
    )
    ( Just $
        Left $
          Local
            3
            (GroupNotExists GroupRemove "group:ghost-group")
    )
