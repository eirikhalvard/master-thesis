{-# LANGUAGE OverloadedLists #-}

import Control.Exception (evaluate)
import Control.Lens
import Example
import qualified Lenses as L
import qualified Merge.ChangeDetection as ChangeDetection
import qualified Merge.CheckPlan as CheckPlan
import qualified Merge.PlanMerging as PlanMerging
import Test.Hspec
import Test.QuickCheck
import Types

baseModificationEvolutionPlan =
  ChangeDetection.constructModificationLevelEP
    . ChangeDetection.flattenEvolutionPlan
    $ baseEvolutionPlan

v1ModificationEvolutionPlan =
  ChangeDetection.constructModificationLevelEP
    . ChangeDetection.flattenEvolutionPlan
    $ v1EvolutionPlan

v2ModificationEvolutionPlan =
  ChangeDetection.constructModificationLevelEP
    . ChangeDetection.flattenEvolutionPlan
    $ v2EvolutionPlan

mergePlan =
  PlanMerging.createMergePlan
    baseModificationEvolutionPlan
    v1ModificationEvolutionPlan
    v2ModificationEvolutionPlan

unifiedMergePlan =
  PlanMerging.unifyMergePlan mergePlan

expectedEvolutionPlanTransformed =
  ChangeDetection.constructModificationLevelEP
    . ChangeDetection.flattenEvolutionPlan
    $ expectedEvolutionPlan

main :: IO ()
main = hspec $ do
  describe "Prelude.head" $ do
    it "returns the first element of a list" $ do
      head [23 ..] `shouldBe` (23 :: Int)

    it "returns the first element of an *arbitrary* list" $
      property $ \x xs -> head (x : xs) == (x :: Int)

    it "throws an exception if used with an empty list" $ do
      evaluate (head []) `shouldThrow` anyException

  describe "Change Detection" $ do
    it "deriving changes at the first timepoint correctly" $ do
      preview (L.plans . ix 0) baseModificationEvolutionPlan
        `shouldBe` Just
          ( Plan
              1
              ( Modifications
                  [
                    ( "feature:vending-machine"
                    , FeatureModification Nothing Nothing (Just (FeatureNameModification "VendingMachine"))
                    )
                  ,
                    ( "feature:beverages"
                    , FeatureAdd "group:vending-machine-group" Mandatory "Beverages"
                    )
                  ,
                    ( "feature:tea"
                    , FeatureAdd "group:beverages-group" Optional "Tea"
                    )
                  ,
                    ( "feature:coffee"
                    , FeatureAdd "group:beverages-group" Optional "Coffee"
                    )
                  ]
                  [ ("group:vending-machine-group", GroupAdd "feature:vending-machine" And)
                  , ("group:beverages-group", GroupAdd "feature:beverages" Or)
                  ]
              )
          )
  describe "Plan Merging" $ do
    it "result of example merge is equal to expected result" $ do
      Right expectedEvolutionPlanTransformed `shouldBe` unifiedMergePlan
