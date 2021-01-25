{-# LANGUAGE FlexibleContexts #-}

module Examples.MergeConflictExample where

import Control.Lens
import Text.Pretty.Simple

import Convertable (ConvertableFromResult, ConvertableInput, convertFromMergeResult)
import Examples.SoundExample
import qualified Lenses as L
import ThreeWayMerge
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

multipleAdd :: MergeInput FlatModificationEvolutionPlan
multipleAdd =
  MergeInput
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
  ( ConvertableInput evolutionPlan FlatModificationEvolutionPlan
  , ConvertableFromResult evolutionPlan
  , Eq evolutionPlan
  , Show evolutionPlan
  ) =>
  MergeInput evolutionPlan ->
  IO ()
showExampleResult mergeInput@(MergeInput name base v1 v2 Nothing) = do
  let result = threeWayMerge mergeInput
  print $ "EXAMPLE " ++ name
  print $ "NO EXPECTED OUTPUT GIVEN" ++ name
  print "ACTUAL RESULT:"
  case result of
    Left err -> pPrint err
    Right model -> pPrint model
showExampleResult mergeInput@(MergeInput name base v1 v2 (Just expected)) = do
  let result = threeWayMerge mergeInput
      converted = fmap (uncurry convertFromMergeResult) result
  print $ "EXAMPLE " ++ name
  if converted == expected
    then do
      print "THE RESULT WERE AS EXPECTED"
      pPrint result
    else do
      print "THE RESULT WERE NOT AS EXPECTED"
      print "EXPECTED RESULT:"
      pPrint expected
      print "ACTUAL RESULT:"
      case result of
        Left err -> pPrint err
        Right model -> pPrint model
