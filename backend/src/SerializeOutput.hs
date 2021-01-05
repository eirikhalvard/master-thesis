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

  let baseModificationEvolutionPlan = deriveChanges baseEvolutionPlan
      v1ModificationEvolutionPlan = deriveChanges v1EvolutionPlan
      v2ModificationEvolutionPlan = deriveChanges v2EvolutionPlan
      mergePlan =
        createMergePlan
          baseModificationEvolutionPlan
          v1ModificationEvolutionPlan
          v2ModificationEvolutionPlan

  print "------- BASE ABSTRACTED EVOLUTION PLAN -------"
  pPrint baseEvolutionPlan

  print "------- BASE MODIFICATION EVOLUTION PLAN -------"
  pPrint baseModificationEvolutionPlan

  print "------- MERGE EVOLUTION PLAN -------"
  pPrint mergePlan
