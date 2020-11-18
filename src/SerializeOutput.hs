module SerializeOutput where

import Control.Lens
import Example
import qualified Lenses as L
import Merge.ChangeDetection
import Types

import Data.Aeson

writeExampleToFile :: FilePath -> IO ()
writeExampleToFile filename = do
  print $ "Writing json to file " ++ filename
  encodeFile filename mergeResult

  print "BASE NORMAL:"
  baseEvolutionPlan
    & L.timePoints
      . traversed
      . L.featureModel
    %%~ print

  print "BASE FLAT:"
  mapM_ print $
    flattenEvolutionPlan baseEvolutionPlan
      ^.. L.timePoints
        . ix 1
        . L.featureModel
        . L.features
        . traversed
  print "----------FEATURE MODIFICATIONS----------"
  mapM_ print $
    deriveChanges baseEvolutionPlan
      ^@.. L.plans . ix 0 . L.transformation . L.features . itraversed
  print "----------GROUP MODIFICATIONS----------"
  mapM_ print $
    deriveChanges baseEvolutionPlan
      ^@.. L.plans . ix 0 . L.transformation . L.groups . itraversed
  where
    mergeResult =
      MergeResult
        [ MergeEvolutionPlan "base" baseEvolutionPlan
        , MergeEvolutionPlan "v1" v1EvolutionPlan
        , MergeEvolutionPlan "v2" v2EvolutionPlan
        , MergeEvolutionPlan "expected" expectedEvolutionPlan
        ]
