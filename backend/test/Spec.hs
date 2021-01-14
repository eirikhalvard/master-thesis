{-# LANGUAGE OverloadedLists #-}

import Control.Exception (evaluate)
import Control.Lens
import Example
import qualified Lenses as L
import qualified Merge.ChangeDetection as ChangeDetection
import qualified Merge.CheckPlan as CheckPlan
import qualified Merge.PlanMerging as PlanMerging
import Merge.Types
import Test.Hspec
import Test.QuickCheck
import Types

baseModificationEvolutionPlan :: ModificationLevelEvolutionPlan FeatureModel'
baseModificationEvolutionPlan =
  ChangeDetection.constructModificationLevelEP
    . ChangeDetection.flattenEvolutionPlan
    $ baseEvolutionPlan

v1ModificationEvolutionPlan :: ModificationLevelEvolutionPlan FeatureModel'
v1ModificationEvolutionPlan =
  ChangeDetection.constructModificationLevelEP
    . ChangeDetection.flattenEvolutionPlan
    $ v1EvolutionPlan

v2ModificationEvolutionPlan :: ModificationLevelEvolutionPlan FeatureModel'
v2ModificationEvolutionPlan =
  ChangeDetection.constructModificationLevelEP
    . ChangeDetection.flattenEvolutionPlan
    $ v2EvolutionPlan

mergePlan :: MergeLevelEvolutionPlan FeatureModel'
mergePlan =
  PlanMerging.createMergePlan
    baseModificationEvolutionPlan
    v1ModificationEvolutionPlan
    v2ModificationEvolutionPlan

unifiedMergePlan :: Either Conflict (ModificationLevelEvolutionPlan FeatureModel')
unifiedMergePlan =
  PlanMerging.unifyMergePlan mergePlan

integratedPlan :: Either Conflict (AbstractedLevelEvolutionPlan FeatureModel')
integratedPlan =
  unifiedMergePlan >>= CheckPlan.integrateAllModifications

expectedEvolutionPlanFlattened :: AbstractedLevelEvolutionPlan FeatureModel'
expectedEvolutionPlanFlattened =
  ChangeDetection.flattenEvolutionPlan expectedEvolutionPlan

expectedEvolutionPlanTransformed :: ModificationLevelEvolutionPlan FeatureModel'
expectedEvolutionPlanTransformed =
  ChangeDetection.constructModificationLevelEP expectedEvolutionPlanFlattened

main :: IO ()
main = hspec $ do
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

  describe "Plan Checking" $ do
    it "checks local and global conflicts and returns no conflicts for example plan" $ do
      Right expectedEvolutionPlanFlattened `shouldBe` integratedPlan
