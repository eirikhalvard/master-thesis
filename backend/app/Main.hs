{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Lens
import Control.Monad (when)
import Data.Aeson (ToJSON, decodeFileStrict)
import Data.List (intercalate)
import qualified Data.Map as M

import Cli (executeParser)
import Convertable
import Examples.GlobalConflictExample
import Examples.LocalConflictExample
import Examples.MergeConflictExample
import Examples.SoundExample
import qualified Lenses as L
import SerializeOutput
import ThreeWayMerge (threeWayMerge)
import Types

------------------------------------------------------------------------
--                              Program                               --
------------------------------------------------------------------------

main :: IO ()
main = runProgram =<< executeParser

runProgram :: CliOptions -> IO ()
runProgram options = do
  case options ^. L.mode of
    GenerateAll ->
      runGenerateAll options
    GenerateOne toGenerate ->
      runGenerateOne options toGenerate
    FromFile filepath ->
      runFromFile options filepath

------------------------------------------------------------------------
--                            Generate All                            --
------------------------------------------------------------------------

runGenerateAll :: CliOptions -> IO ()
runGenerateAll = mergeAll

------------------------------------------------------------------------
--                            Generate One                            --
------------------------------------------------------------------------

runGenerateOne :: CliOptions -> String -> IO ()
runGenerateOne options toGenerate =
  let errMsg =
        "Example to generate (toGenerate option) not found!"
          ++ "Try one of the following:\n"
          ++ (intercalate ", " . fmap show . M.keys $ mergeData)
   in maybe
        (print errMsg)
        (mergeSingle options)
        (M.lookup toGenerate mergeData)

------------------------------------------------------------------------
--                             From File                              --
------------------------------------------------------------------------

runFromFile :: CliOptions -> FilePath -> IO ()
runFromFile options filepath =
  let errMsg = "Could not parse file! something went wrong. Did you set the right input format?"
   in mergeInputFromFile options filepath
        >>= maybe (print errMsg) (mergeSingle options)

mergeInputFromFile :: CliOptions -> FilePath -> IO (Maybe MergeInput)
mergeInputFromFile options filepath =
  case options ^. L.fromType of
    TreeUserType ->
      (fmap . fmap) TreeUser (decodeFileStrict filepath)
    FlatUserType ->
      (fmap . fmap) FlatUser (decodeFileStrict filepath)
    FlatModificationType ->
      (fmap . fmap) FlatModification (decodeFileStrict filepath)

------------------------------------------------------------------------
--                       Full Merge Algorithms                        --
------------------------------------------------------------------------

mergeSingle :: CliOptions -> MergeInput -> IO ()
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

mergeAll :: CliOptions -> IO ()
mergeAll options = do
  elmData <- traverse handleSingleMerge (M.elems mergeData)
  maybeWriteToElm options elmData
  where
    handleSingleMerge mergeInput = do
      -- we convert from and to every representation
      -- this is still efficient because haskell is lazy
      let (treeUserInput, flatUserInput, flatModificationInput) =
            getAllMergeInputRepresentations mergeInput

          mergeOutput = threeWayMerge flatModificationInput

          (treeUserResult, flatUserResult, flatModificationResult) =
            getAllMergeOutputRepresentations mergeOutput

      case options ^. L.toType of
        TreeUserType ->
          maybePrintResult options treeUserInput treeUserResult
        FlatUserType ->
          maybePrintResult options flatUserInput flatUserResult
        FlatModificationType ->
          maybePrintResult options flatModificationInput flatModificationResult

      return (treeUserInput, treeUserResult)

------------------------------------------------------------------------
--                              Helpers                               --
------------------------------------------------------------------------

-- converting between representations

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

-- printing and file writing

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

------------------------------------------------------------------------
--                                Data                                --
------------------------------------------------------------------------

elmDataPath :: FilePath
elmDataPath = "../frontend/data/elm-input.json"

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
