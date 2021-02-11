{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Lens
import Control.Monad
import Data.Aeson (ToJSON, decodeFileStrict, encodeFile)
import qualified Data.Map as M
import Text.Pretty.Simple

import Cli
import Convertable
import Examples.GlobalConflictExample
import Examples.LocalConflictExample
import Examples.MergeConflictExample
import Examples.SoundExample
import qualified Lenses as L
import SerializeOutput
import ThreeWayMerge
import Types

mergeData :: M.Map String MergeInput
mergeData =
  M.fromList
    [ ("SoundExample", TreeUser soundExample)
    , ("ConflictMultipleAdd", FlatModification multipleAdd)
    , ("ConflictRemoveAndChange", FlatModification removeAndChangeModification)
    , ("MovedAddition", FlatModification movedFeatureAddition)
    , ("ConflictingAdditionMove", FlatModification conflictingAdditionMove)
    , ("ConflictingGroupRemove", FlatModification conflictingGroupRemove)
    , ("MoveGroupCycle", FlatModification groupMoveCycle)
    , ("WellFormedViolation", FlatModification violatingFeatureWellFormed)
    , ("MissingParentFeature", FlatModification missingParentFeature)
    ]

mergeAll ::
  Bool ->
  Maybe FilePath ->
  IO ()
mergeAll shouldPrint maybeElmFilePath = do
  elmExamples <- ElmDataExamples <$> traverse handleSingleMerge (M.elems mergeData)
  mapM_ (`encodeFile` elmExamples) maybeElmFilePath
  where
    handleSingleMerge :: MergeInput -> IO ElmMergeExample
    handleSingleMerge mergeInput = undefined

-- case mergeInput of
--   TreeUser mergeInputData -> do
--     let mergeOutput = threeWayMerge mergeInputData
--         convertedInputData :: MergeInputData FlatModificationEvolutionPlan
--         convertedInputData = convertFrom <$> mergeInputData
--         convertedResult :: MergeResult FlatModificationEvolutionPlan
--         convertedResult = fmap (uncurry convertFromMergeResult) mergeOutput
--     when shouldPrint (printResult convertedInputData convertedResult)
--     return $ createElmExample mergeInputData mergeOutput
--   FlatUser mergeInputData -> do
--     let mergeOutput = threeWayMerge mergeInputData
--         convertedInputData :: MergeInputData FlatModificationEvolutionPlan
--         convertedInputData = convertFrom <$> mergeInputData
--         convertedResult :: MergeResult FlatModificationEvolutionPlan
--         convertedResult = fmap (uncurry convertFromMergeResult) mergeOutput
--     when shouldPrint (printResult convertedInputData convertedResult)
--     return $ createElmExample mergeInputData mergeOutput
--   FlatModification mergeInputData -> do
--     let mergeOutput = threeWayMerge mergeInputData
--         convertedInputData :: MergeInputData FlatModificationEvolutionPlan
--         convertedInputData = convertFrom <$> mergeInputData
--         convertedResult :: MergeResult FlatModificationEvolutionPlan
--         convertedResult = fmap (uncurry convertFromMergeResult) mergeOutput
--     when shouldPrint (printResult convertedInputData convertedResult)
--     return $ createElmExample mergeInputData mergeOutput

elmDataPath :: FilePath
elmDataPath = "../frontend/data/elm-input.json"

------------------------------------------------------------------------
--                            Merge Single                            --
------------------------------------------------------------------------

mergeSingle ::
  CliOptions ->
  MergeInput ->
  IO ()
mergeSingle options mergeInput = do
  -- we convert from and to every representation
  -- this is still efficient because haskell is lazy
  let (treeUserInput, flatUserInput, flatModificationInput) =
        getAllMergeInputRepresentations mergeInput

      mergeOutput = threeWayMerge flatModificationInput

      (treeUserResult, flatUserResult, flatModificationResult) =
        getAllMergeOutputRepresentations mergeOutput

  maybeWriteToElm options [(treeUserInput, treeUserResult)]

  case options ^. L.toType of
    TreeUserType -> do
      maybePrintResult options treeUserInput treeUserResult
      maybeWriteToFile options treeUserResult
    FlatUserType -> do
      maybePrintResult options flatUserInput flatUserResult
      maybeWriteToFile options flatUserResult
    FlatModificationType -> do
      maybePrintResult options flatModificationInput flatModificationResult
      maybeWriteToFile options flatModificationResult

runProgram :: CliOptions -> IO ()
runProgram options = do
  case options ^. L.mode of
    GenerateAll ->
      undefined
    GenerateOne toGenerate ->
      undefined
    FromFile filepath ->
      undefined

mergeInputFromFile :: CliOptions -> FilePath -> IO (Maybe MergeInput)
mergeInputFromFile options filepath =
  case options ^. L.fromType of
    TreeUserType ->
      (fmap . fmap) TreeUser (decodeFileStrict filepath)
    FlatUserType ->
      (fmap . fmap) FlatUser (decodeFileStrict filepath)
    FlatModificationType ->
      (fmap . fmap) FlatModification (decodeFileStrict filepath)

main :: IO ()
main = do
  runProgram =<< executeParser

  -- mergeOne "SoundExample"
  mergeAll True (Just "../frontend/data/elm-input.json")
  return ()
  where

-- mergeOne :: String -> IO (Maybe (MergeResult TreeUserEvolutionPlan))
-- mergeOne key = case M.lookup key mergeData of
--   Nothing -> print "key not found!" >> return Nothing
--   Just mergeInput -> do
--     Just
--       <$> mergeSingle
--         True
--         (Just "../frontend/data/elm-input.json")
--         Nothing
--         mergeInput
------------------------------------------------------------------------
--                             Merge All                              --
------------------------------------------------------------------------

------------------------------------------------------------------------
--                              Helpers                               --
------------------------------------------------------------------------

getAllMergeInputRepresentations ::
  MergeInput ->
  ( MergeInputData TreeUserEvolutionPlan
  , MergeInputData FlatUserEvolutionPlan
  , MergeInputData FlatModificationEvolutionPlan
  )
getAllMergeInputRepresentations mergeInput =
  case mergeInput of
    TreeUser input ->
      ( toTreeUser <$> input
      , toFlatUser <$> input
      , toFlatModification <$> input
      )
    FlatUser input ->
      ( toTreeUser <$> input
      , toFlatUser <$> input
      , toFlatModification <$> input
      )
    FlatModification input ->
      ( toTreeUser <$> input
      , toFlatUser <$> input
      , toFlatModification <$> input
      )

getAllMergeOutputRepresentations ::
  MergeOutput ->
  ( MergeResult TreeUserEvolutionPlan
  , MergeResult FlatUserEvolutionPlan
  , MergeResult FlatModificationEvolutionPlan
  )
getAllMergeOutputRepresentations mergeOutput =
  ( uncurry convertFromMergeResult <$> mergeOutput
  , uncurry convertFromMergeResult <$> mergeOutput
  , uncurry convertFromMergeResult <$> mergeOutput
  )

maybePrintResult ::
  (Eq evolutionPlan, Show evolutionPlan) =>
  CliOptions ->
  MergeInputData evolutionPlan ->
  MergeResult evolutionPlan ->
  IO ()
maybePrintResult options mergeInput mergeResult =
  when (options ^. L.print) (printResult mergeInput mergeResult)

maybeWriteToElm ::
  CliOptions ->
  [ ( MergeInputData TreeUserEvolutionPlan
    , MergeResult TreeUserEvolutionPlan
    )
  ] ->
  IO ()
maybeWriteToElm options elmData =
  when
    (options ^. L.generateElm)
    (writeToElm elmDataPath elmData)

maybeWriteToFile ::
  ToJSON evolutionPlan =>
  CliOptions ->
  MergeResult evolutionPlan ->
  IO ()
maybeWriteToFile options result =
  mapM_
    ( \filepath -> case result of
        Left _ ->
          print "Could not write to file, conflict occured!"
        Right resultingEvolutionPlan ->
          writeResultToFile filepath resultingEvolutionPlan
    )
    (options ^. L.toFile)
