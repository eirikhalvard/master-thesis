module SerializeOutput where

import Example
import Merge.ChangeDetection
import Merge.PlanMerging
import Text.Pretty.Simple (pPrint)
import Types

import Data.Aeson

writeExampleToFile :: FilePath -> IO ()
writeExampleToFile filename = do
  print $ "Writing json to file " ++ filename
  encodeFile filename $
    MergeResult
      [ MergeEvolutionPlan "Base" baseEvolutionPlan
      , MergeEvolutionPlan "Version 1" v1EvolutionPlan
      , MergeEvolutionPlan "Version 2" v2EvolutionPlan
      , MergeEvolutionPlan "Expected" expectedEvolutionPlan
      ]

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
      expectedEvolutionPlanTransformed =
        constructModificationLevelEP
          . flattenEvolutionPlan
          $ expectedEvolutionPlan

  print "------- BASE ABSTRACTED EVOLUTION PLAN -------"
  pPrint baseEvolutionPlan

  print "------- BASE MODIFICATION EVOLUTION PLAN -------"
  pPrint baseModificationEvolutionPlan

  print "------- MERGE EVOLUTION PLAN -------"
  pPrint mergePlan

  print "------- UNIFIED MERGE EVOLUTION PLAN -------"
  pPrint unifiedMergePlan

  print "------- EXPECTED EVOLUTION PLAN -------"
  pPrint expectedEvolutionPlanTransformed

  print "------- UNIFIED == EXPECTED -------"
  print $ Right expectedEvolutionPlanTransformed == unifiedMergePlan
