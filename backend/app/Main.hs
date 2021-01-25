module Main where

import qualified Data.Map as M

import Convertable
import Data.Aeson (ToJSON)
import Examples.MergeConflictExample (multipleAdd)
import Examples.SoundExample
import SerializeOutput
import ThreeWayMerge
import Types

mergeData :: M.Map String MergeInput
mergeData =
  M.fromList
    [ ("SoundExample", TreeUser soundExample)
    , ("ConflictMultipleAdd", FlatModification multipleAdd)
    ]

mergeAll ::
  ConvertableFromResult outputEvolutionPlan =>
  Bool ->
  Bool ->
  IO [MergeResult outputEvolutionPlan]
mergeAll shouldPrint shouldWriteToElm =
  traverse handleSingleMerge $ M.elems mergeData
  where
    handleSingleMerge = undefined

mergeSingle ::
  (ConvertableFromResult outputEvolutionPlan, ToJSON outputEvolutionPlan) =>
  Bool ->
  Bool ->
  Maybe FilePath ->
  MergeInput ->
  IO (MergeResult outputEvolutionPlan)
mergeSingle shouldPrint shouldWriteToElm maybeFilepath mergeInput = do
  let mergeOutput = case mergeInput of
        TreeUser ep -> threeWayMerge ep
        FlatUser ep -> threeWayMerge ep
        FlatModification ep -> threeWayMerge ep
      convertedResult = fmap (uncurry convertFromMergeResult) mergeOutput

  if shouldPrint
    then return ()
    else return ()

  if shouldWriteToElm
    then return ()
    else return ()

  case maybeFilepath of
    Nothing -> return ()
    Just filepath -> case convertedResult of
      Left _ -> print "Could not write to file, conflict occured!"
      Right resultingEvolutionPlan -> writeResultToFile filepath resultingEvolutionPlan

  return convertedResult

main :: IO ()
main = do
  writeElmExamplesToFile
    "../frontend/data/elm-input.json"
    [(soundExample, threeWayMerge soundExample)]
