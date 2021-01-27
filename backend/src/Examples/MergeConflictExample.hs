{-# LANGUAGE FlexibleContexts #-}

module Examples.MergeConflictExample where

import Control.Lens

import Examples.SoundExample
import qualified Lenses as L
import Types

-- data Conflict
--   = Merge Time MergeConflict
--   | Local Time LocalConflict
--   | Global Time GlobalConflict
--   | Panic Time String
--   deriving (Show, Eq)

-- data MergeConflict
--   = FeatureConflict (BothChange FeatureModification)
--   | GroupConflict (BothChange GroupModification)
--   deriving (Show, Eq)

multipleAdd :: MergeInputData FlatModificationEvolutionPlan
multipleAdd =
  MergeInputData
    "Multiple Add - Conflict"
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
