module SerializeOutput where

import Examples.MergeConflictExample
import Examples.SoundExample
import Merge.ChangeDetection
import Merge.PlanMerging
import Text.Pretty.Simple (pPrint)
import ThreeWayMerge (threeWayMerge)
import Types

import Data.Aeson (encodeFile)
import Merge.CheckPlan (integrateAllModifications)

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
      [ MergeEvolutionPlan "Base" baseEvolutionPlan
      , MergeEvolutionPlan "Version 1" v1EvolutionPlan
      , MergeEvolutionPlan "Version 2" v2EvolutionPlan
      , MergeEvolutionPlan "Expected" expectedEvolutionPlan
      , MergeEvolutionPlan
          "Actual"
          $ either
            (const $ error "merge not successful")
            id
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
