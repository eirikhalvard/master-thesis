module SerializeOutput where

import Examples.MergeConflictExample
import Examples.SoundExample
import Merge.ChangeDetection
import Merge.CheckPlan (integrateAndCheckModifications)
import Merge.PlanMerging
import ThreeWayMerge (conflictErrorMsg, threeWayMerge)
import Types

import Data.Aeson (encodeFile)

writeExampleToFile :: FilePath -> IO ()
writeExampleToFile filename = do
  let baseModificationEvolutionPlan =
        deriveSoundModifications
          . flattenSoundEvolutionPlan
          $ baseEvolutionPlan
      v1ModificationEvolutionPlan =
        deriveSoundModifications
          . flattenSoundEvolutionPlan
          $ v1EvolutionPlan
      v2ModificationEvolutionPlan =
        deriveSoundModifications
          . flattenSoundEvolutionPlan
          $ v2EvolutionPlan
      mergePlan =
        createMergePlan
          baseModificationEvolutionPlan
          v1ModificationEvolutionPlan
          v2ModificationEvolutionPlan
      unifiedMergePlan =
        unifyMergePlan mergePlan
      checkedAndIntegratedPlan =
        unifiedMergePlan >>= integrateAndCheckModifications
      expectedEvolutionPlanTransformed =
        deriveSoundModifications
          . flattenSoundEvolutionPlan
          $ expectedEvolutionPlan
      actualResult =
        threeWayMerge
          baseEvolutionPlan
          v1EvolutionPlan
          v2EvolutionPlan

  print $ "Writing json to file " ++ filename
  encodeFile filename $
    ElmMergeResult
      "Sound Example"
      [ ElmNamedEvolutionPlan "Base" $ EvolutionPlanResult baseEvolutionPlan
      , ElmNamedEvolutionPlan "Version 1" $ EvolutionPlanResult v1EvolutionPlan
      , ElmNamedEvolutionPlan "Version 2" $ EvolutionPlanResult v2EvolutionPlan
      , ElmNamedEvolutionPlan "Expected" $ EvolutionPlanResult expectedEvolutionPlan
      , ElmNamedEvolutionPlan "Actual" $
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
