{-# LANGUAGE OverloadedLists #-}

import Control.Lens
import Examples.SoundExample
import qualified Lenses as L
import qualified Merge.ChangeDetection as ChangeDetection
import qualified Merge.CheckPlan as CheckPlan
import qualified Merge.PlanMerging as PlanMerging
import Test.Hspec
import ThreeWayMerge (threeWayMerge)
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

unflattenedPlan :: Either Conflict (AbstractedLevelEvolutionPlan FeatureModel)
unflattenedPlan =
  integratedPlan >>= CheckPlan.unflattenEvolutionPlan

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

  describe "Change Detection" $
    it "flattens and constructs modification levle correctly fro sound example" $ do
      ChangeDetection.constructModificationLevelEP
        (ChangeDetection.flattenEvolutionPlan baseEvolutionPlan)
        `shouldBe` baseConstructedEvolutionPlan
      ChangeDetection.constructModificationLevelEP
        (ChangeDetection.flattenEvolutionPlan v1EvolutionPlan)
        `shouldBe` v1ConstructedEvolutionPlan
      ChangeDetection.constructModificationLevelEP
        (ChangeDetection.flattenEvolutionPlan v2EvolutionPlan)
        `shouldBe` v2ConstructedEvolutionPlan
      ChangeDetection.constructModificationLevelEP
        (ChangeDetection.flattenEvolutionPlan expectedEvolutionPlan)
        `shouldBe` expectedConstructedEvolutionPlan
  describe "Plan Merging" $ do
    it "result of example merge is equal to expected result" $ do
      Right expectedEvolutionPlanTransformed `shouldBe` unifiedMergePlan

  describe "Plan Checking" $ do
    it "checks local and global conflicts and returns no conflicts for example plan" $ do
      Right expectedEvolutionPlanFlattened `shouldBe` integratedPlan
    it "unflattens correct plan correctly" $ do
      Right expectedEvolutionPlan `shouldBe` unflattenedPlan
    it "complete three way merge works for example" $ do
      threeWayMerge baseEvolutionPlan v1EvolutionPlan v2EvolutionPlan
        `shouldBe` Right expectedEvolutionPlan
