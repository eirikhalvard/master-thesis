{-# LANGUAGE FlexibleContexts #-}

module Examples.MergeConflictExample where

import Control.Lens
import Text.Pretty.Simple

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

showExampleResult ::
  ( Eq evolutionPlan
  , Show evolutionPlan
  ) =>
  MergeInputData evolutionPlan ->
  MergeResult evolutionPlan ->
  IO ()
showExampleResult mergeInput mergeResult = do
  print $ "EXAMPLE " ++ mergeInput ^. L.name
  case mergeInput ^. L.maybeExpected of
    Nothing -> do
      print $ "NO EXPECTED OUTPUT GIVEN"
    Just expected -> do
      if expected == mergeResult
        then do
          print "THE RESULT WERE AS EXPECTED"
        else do
          print "THE RESULT WERE NOT AS EXPECTED"
          print "EXPECTED RESULT:"
          pPrint expected

  print "ACTUAL RESULT:"
  case mergeResult of
    Left err -> pPrint err
    Right model -> pPrint model
