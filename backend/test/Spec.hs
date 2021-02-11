{-# LANGUAGE OverloadedLists #-}

import Control.Lens
import Test.Hspec

import Convertable
import Examples.SoundExample
import qualified Lenses as L
import qualified Merge.ChangeDetection as ChangeDetection
import qualified Merge.CheckPlan as CheckPlan
import qualified Merge.PlanMerging as PlanMerging
import ThreeWayMerge (threeWayMerge)
import Types

baseModificationEvolutionPlan :: FlatModificationEvolutionPlan
baseModificationEvolutionPlan =
  ChangeDetection.deriveSoundModifications
    . ChangeDetection.flattenSoundEvolutionPlan
    $ baseEvolutionPlan

v1ModificationEvolutionPlan :: FlatModificationEvolutionPlan
v1ModificationEvolutionPlan =
  ChangeDetection.deriveSoundModifications
    . ChangeDetection.flattenSoundEvolutionPlan
    $ v1EvolutionPlan

v2ModificationEvolutionPlan :: FlatModificationEvolutionPlan
v2ModificationEvolutionPlan =
  ChangeDetection.deriveSoundModifications
    . ChangeDetection.flattenSoundEvolutionPlan
    $ v2EvolutionPlan

mergePlan :: TransformationEvolutionPlan DiffResult FlatFeatureModel
mergePlan =
  PlanMerging.createMergePlan
    baseModificationEvolutionPlan
    v1ModificationEvolutionPlan
    v2ModificationEvolutionPlan

unifiedMergePlan :: Either Conflict FlatModificationEvolutionPlan
unifiedMergePlan =
  PlanMerging.unifyMergePlan mergePlan

integratedPlan :: Either Conflict FlatUserEvolutionPlan
integratedPlan =
  unifiedMergePlan >>= CheckPlan.integrateAndCheckModifications

unflattenedPlan :: Either Conflict TreeUserEvolutionPlan
unflattenedPlan =
  CheckPlan.unflattenSoundEvolutionPlan <$> integratedPlan

expectedEvolutionPlanFlattened :: FlatUserEvolutionPlan
expectedEvolutionPlanFlattened =
  ChangeDetection.flattenSoundEvolutionPlan expectedEvolutionPlan

expectedEvolutionPlanTransformed :: FlatModificationEvolutionPlan
expectedEvolutionPlanTransformed =
  ChangeDetection.deriveSoundModifications expectedEvolutionPlanFlattened

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
      ChangeDetection.deriveSoundModifications
        (ChangeDetection.flattenSoundEvolutionPlan baseEvolutionPlan)
        `shouldBe` baseConstructedEvolutionPlan
      ChangeDetection.deriveSoundModifications
        (ChangeDetection.flattenSoundEvolutionPlan v1EvolutionPlan)
        `shouldBe` v1ConstructedEvolutionPlan
      ChangeDetection.deriveSoundModifications
        (ChangeDetection.flattenSoundEvolutionPlan v2EvolutionPlan)
        `shouldBe` v2ConstructedEvolutionPlan
      ChangeDetection.deriveSoundModifications
        (ChangeDetection.flattenSoundEvolutionPlan expectedEvolutionPlan)
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
      uncurry convertFromMergeResult <$> threeWayMerge soundExample
        `shouldBe` Right expectedEvolutionPlan
