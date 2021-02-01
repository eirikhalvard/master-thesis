{-# LANGUAGE FlexibleContexts #-}

module Examples.MergeConflictExample where

import Control.Lens

import Examples.SoundExample
import qualified Lenses as L
import Types

-- Addition and Addition Merge Conflict
-- Each version is adding a modification to the same feature
-- v1 is adding a remove, and v2 is adding a change
multipleAdd :: MergeInputData FlatModificationEvolutionPlan
multipleAdd =
  MergeInputData
    "Conflict, Merge - Multiple Add"
    baseConstructedEvolutionPlan
    ( v1ConstructedEvolutionPlan
        & L.plans
          . traversed
          . filtered (has $ L.timePoint . only 2)
          . L.transformation
          . L.features
          . at "feature:tea"
          ?~ FeatureRemove
    )
    ( v2ConstructedEvolutionPlan
        & L.plans
          . traversed
          . filtered (has $ L.timePoint . only 2)
          . L.transformation
          . L.features
          . at "feature:tea"
          ?~ FeatureModification
            Nothing
            Nothing
            (Just (FeatureNameModification "Tea Drink"))
    )
    ( Just $
        Left $
          Merge
            2
            ( FeatureConflict $
                BothChangeWithoutBase
                  (AddedModification FeatureRemove)
                  ( AddedModification
                      ( FeatureModification
                          Nothing
                          Nothing
                          (Just (FeatureNameModification "Tea Drink"))
                      )
                  )
            )
    )

-- Remove and Change Merge Conflict
-- Each version is changing an existing modification on the same feature
-- v1 is removing a AddFeature, and v2 is changing the fields of the AddFeature
removeAndChangeModification :: MergeInputData FlatModificationEvolutionPlan
removeAndChangeModification =
  MergeInputData
    "Conflict, Merge - Change and Remove"
    baseConstructedEvolutionPlan
    v1ConstructedEvolutionPlan
    ( v2ConstructedEvolutionPlan
        & L.plans
          . traversed
          . filtered (has $ L.timePoint . only 3)
          . L.transformation
          . L.features
          . ix "feature:soy-milk"
          .~ FeatureAdd "group:milk-type-group" Mandatory "Soy Milk"
    )
    ( Just $
        Left $
          Merge
            3
            ( FeatureConflict $
                BothChangeWithBase
                  (FeatureAdd "group:milk-type-group" Optional "Soy Milk")
                  RemovedModification
                  ( ChangedModification
                      ( FeatureAdd
                          "group:milk-type-group"
                          Mandatory
                          "Soy Milk"
                      )
                  )
            )
    )
