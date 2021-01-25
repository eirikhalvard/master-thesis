{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Monad
import Data.Aeson (ToJSON)
import qualified Data.Map as M

import Convertable
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
  ( ConvertableFromResult outputEvolutionPlan
  , ConvertableInput TreeUserEvolutionPlan outputEvolutionPlan
  , ConvertableInput FlatUserEvolutionPlan outputEvolutionPlan
  , ConvertableInput FlatModificationEvolutionPlan outputEvolutionPlan
  , Eq outputEvolutionPlan
  , Show outputEvolutionPlan
  , ToJSON outputEvolutionPlan
  ) =>
  Bool ->
  Maybe FilePath ->
  Maybe FilePath ->
  MergeInput ->
  IO (MergeResult outputEvolutionPlan)
mergeSingle shouldPrint maybeElmFilePath maybeFilepath mergeInput = do
  let mergeOutput = case mergeInput of
        TreeUser mergeInputData -> threeWayMerge mergeInputData
        FlatUser mergeInputData -> threeWayMerge mergeInputData
        FlatModification mergeInputData -> threeWayMerge mergeInputData
      convertedInputData = case mergeInput of
        TreeUser mergeInputData -> convertFrom <$> mergeInputData
        FlatUser mergeInputData -> convertFrom <$> mergeInputData
        FlatModification mergeInputData -> convertFrom <$> mergeInputData
      convertedResult = fmap (uncurry convertFromMergeResult) mergeOutput

  when shouldPrint (printResult convertedInputData convertedResult)

  case maybeElmFilePath of
    Nothing -> return ()
    Just filepath ->
      case mergeInput of
        TreeUser mergeInputData ->
          writeElmExamplesToFile filepath [(mergeInputData, mergeOutput)]
        FlatUser mergeInputData ->
          writeElmExamplesToFile filepath [(mergeInputData, mergeOutput)]
        FlatModification mergeInputData ->
          writeElmExamplesToFile filepath [(mergeInputData, mergeOutput)]

  mapM_
    ( \filepath -> case convertedResult of
        Left _ -> print "Could not write to file, conflict occured!"
        Right resultingEvolutionPlan -> writeResultToFile filepath resultingEvolutionPlan
    )
    maybeFilepath

  return convertedResult

main :: IO ()
main = do
  resultingSoundExample <- mergeOne "SoundExample"
  return ()
  where
    mergeOne :: String -> IO (Maybe (MergeResult TreeUserEvolutionPlan))
    mergeOne key = case M.lookup key mergeData of
      Nothing -> print "key not found!" >> return Nothing
      Just mergeInput -> do
        Just
          <$> mergeSingle
            True
            (Just "../frontend/data/elm-input.json")
            Nothing
            mergeInput
