module Examples.MergeConflictExample where

import Control.Lens
import Examples.SoundExample
import qualified Lenses as L
import Merge.Types
import Text.Pretty.Simple
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

data ExampleResult = ExampleResult
  { base :: ModificationLevelEvolutionPlan FeatureModel'
  , v1 :: ModificationLevelEvolutionPlan FeatureModel'
  , v2 :: ModificationLevelEvolutionPlan FeatureModel'
  , expected :: Either Conflict (AbstractedLevelEvolutionPlan FeatureModel)
  }

multipleAdd :: ExampleResult
multipleAdd =
  ExampleResult
    { base = baseConstructedEvolutionPlan
    , v1 =
        v1ConstructedEvolutionPlan
          & L.plans
            . traversed
            . filtered (has $ L.timePoint . only 2)
            . L.transformation
            . L.features
            . at "feature:tea"
            ?~ FeatureRemove
    , v2 =
        v2ConstructedEvolutionPlan
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
    , expected =
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
    }

showExampleResult :: ExampleResult -> IO ()
showExampleResult (ExampleResult base v1 v2 expected) = do
  let result = threeWayMerge' base v1 v2
  if result == expected
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
