{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}

module SerializeOutput where

import Examples.MergeConflictExample
import Examples.SoundExample
import Merge.ChangeDetection
import Merge.CheckPlan (integrateAllModifications)
import Merge.PlanMerging
import Merge.Types
import ThreeWayMerge (conflictErrorMsg, threeWayMerge)
import Types

import Data.Aeson
import GHC.Generics

------------------------------------------------------------------------
--                   Merge Result And Json Encoding                   --
------------------------------------------------------------------------

data MergeExamples = MergeExamples
  { _examples :: [MergeResult]
  }
  deriving (Show, Eq, Read, Generic)

data MergeResult = MergeResult
  { _name :: String
  , _evolutionPlans :: [MergeEvolutionPlan]
  }
  deriving (Show, Eq, Read, Generic)

data MergeEvolutionPlan = MergeEvolutionPlan
  { _name :: String
  , _mergeData :: MergeData
  }
  deriving (Show, Eq, Read, Generic)

data MergeData
  = EvolutionPlanResult (UserLevelEvolutionPlan TreeFeatureModel)
  | ConflictResult String
  deriving (Show, Eq, Read, Generic)

customAesonOptions :: Options
customAesonOptions = defaultOptions{fieldLabelModifier = tail}

instance ToJSON MergeResult where
  toJSON = genericToJSON customAesonOptions
  toEncoding = genericToEncoding customAesonOptions

instance ToJSON MergeEvolutionPlan where
  toJSON = genericToJSON customAesonOptions
  toEncoding = genericToEncoding customAesonOptions

instance ToJSON MergeData where
  toJSON = genericToJSON customAesonOptions
  toEncoding = genericToEncoding customAesonOptions

instance ToJSON (UserLevelEvolutionPlan TreeFeatureModel) where
  toJSON = genericToJSON customAesonOptions
  toEncoding = genericToEncoding customAesonOptions

instance ToJSON (TimePoint TreeFeatureModel) where
  toJSON = genericToJSON customAesonOptions
  toEncoding = genericToEncoding customAesonOptions

instance ToJSON TreeFeatureModel where
  toJSON = genericToJSON customAesonOptions
  toEncoding = genericToEncoding customAesonOptions

instance ToJSON TreeFeature where
  toJSON = genericToJSON customAesonOptions
  toEncoding = genericToEncoding customAesonOptions

instance ToJSON TreeGroup where
  toJSON = genericToJSON customAesonOptions
  toEncoding = genericToEncoding customAesonOptions

instance ToJSON FeatureType where
  toJSON = genericToJSON customAesonOptions
  toEncoding = genericToEncoding customAesonOptions

instance ToJSON GroupType where
  toJSON = genericToJSON customAesonOptions
  toEncoding = genericToEncoding customAesonOptions

instance ToJSON evolutionPlan => ToJSON (MergeArtifact evolutionPlan) where
  toJSON = genericToJSON customAesonOptions
  toEncoding = genericToEncoding customAesonOptions

writeExampleToFile :: FilePath -> IO ()
writeExampleToFile filename = do
  let baseModificationEvolutionPlan =
        constructModificationLevelEP
          . flattenEvolutionPlan
          $ baseEvolutionPlan
      v1ModificationEvolutionPlan =
        constructModificationLevelEP
          . flattenEvolutionPlan
          $ v1EvolutionPlan
      v2ModificationEvolutionPlan =
        constructModificationLevelEP
          . flattenEvolutionPlan
          $ v2EvolutionPlan
      mergePlan =
        createMergePlan
          baseModificationEvolutionPlan
          v1ModificationEvolutionPlan
          v2ModificationEvolutionPlan
      unifiedMergePlan =
        unifyMergePlan mergePlan
      checkedAndIntegratedPlan =
        unifiedMergePlan >>= integrateAllModifications
      expectedEvolutionPlanTransformed =
        constructModificationLevelEP
          . flattenEvolutionPlan
          $ expectedEvolutionPlan
      actualResult =
        threeWayMerge
          baseEvolutionPlan
          v1EvolutionPlan
          v2EvolutionPlan

  print $ "Writing json to file " ++ filename
  encodeFile filename $
    MergeResult
      "Sound Example"
      [ MergeEvolutionPlan "Base" $ EvolutionPlanResult baseEvolutionPlan
      , MergeEvolutionPlan "Version 1" $ EvolutionPlanResult v1EvolutionPlan
      , MergeEvolutionPlan "Version 2" $ EvolutionPlanResult v2EvolutionPlan
      , MergeEvolutionPlan "Expected" $ EvolutionPlanResult expectedEvolutionPlan
      , MergeEvolutionPlan "Actual" $
          either
            (ConflictResult . conflictErrorMsg)
            EvolutionPlanResult
            actualResult
      ]
  runFaultyTests

-- print "------- BASE ABSTRACTED EVOLUTION PLAN -------"
-- pPrint baseEvolutionPlan

-- print "------- BASE MODIFICATION EVOLUTION PLAN -------"
-- pPrint baseModificationEvolutionPlan

-- print "------- MERGE EVOLUTION PLAN -------"
-- pPrint mergePlan

-- print "------- UNIFIED MERGE EVOLUTION PLAN -------"
-- pPrint unifiedMergePlan

-- print "------- EXPECTED EVOLUTION PLAN -------"
-- pPrint expectedEvolutionPlanTransformed

-- print "------- UNIFIED == EXPECTED -------"
-- print $ Right expectedEvolutionPlanTransformed == unifiedMergePlan

-- print "------- CHECKED AND INTEGRATED -------"
-- pPrint $ checkedAndIntegratedPlan

runFaultyTests :: IO ()
runFaultyTests = do
  showExampleResult multipleAdd
