module Program where

import Control.Lens
import Control.Monad (when)
import Data.Aeson (ToJSON, decodeFileStrict, encodeFile)
import Data.List (intercalate)
import qualified Data.Map as M

import Cli (executeParser)
import Convertable
import Examples.Examples
import qualified Lenses as L
import SerializeOutput
import ThreeWayMerge (threeWayMerge)
import Types

------------------------------------------------------------------------
--                              Program                               --
------------------------------------------------------------------------

runProgram :: CliOptions -> IO ()
runProgram options = do
  case options ^. L.mode of
    GenerateAll ->
      runGenerateAll options
    GenerateOne toGenerate ->
      runGenerateOne options toGenerate
    FromFile filePath ->
      runFromFile options filePath

elmDataPath :: FilePath
elmDataPath = "../frontend/data/elm-input.json"

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
        (putStrLn errMsg)
        (mergeSingle options)
        (M.lookup toGenerate mergeData)

------------------------------------------------------------------------
--                             From File                              --
------------------------------------------------------------------------

runFromFile :: CliOptions -> FilePath -> IO ()
runFromFile options filePath =
  let errMsg = "Could not parse file! something went wrong. Did you set the right input format?"
   in mergeInputFromFile options filePath
        >>= maybe (putStrLn errMsg) (mergeSingle options)

mergeInputFromFile :: CliOptions -> FilePath -> IO (Maybe MergeInput)
mergeInputFromFile options filePath =
  case options ^. L.fromType of
    TreeUserType ->
      (fmap . fmap) TreeUser (decodeFileStrict filePath)
    FlatUserType ->
      (fmap . fmap) FlatUser (decodeFileStrict filePath)
    FlatModificationType ->
      (fmap . fmap) FlatModification (decodeFileStrict filePath)

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
  ( uncurry fromMergeOutput <$> mergeOutput
  , uncurry fromMergeOutput <$> mergeOutput
  , uncurry fromMergeOutput <$> mergeOutput
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
    ( \filePath -> case result of
        Left _ ->
          print "Could not write to file, conflict occured!"
        Right resultingEvolutionPlan ->
          writeResultToFile filePath resultingEvolutionPlan
    )
    (options ^. L.toFile)

------------------------------------------------------------------------
--                           Write To File                            --
------------------------------------------------------------------------

writeExampleToFile :: FilePath -> String -> EvolutionPlanType -> IO ()
writeExampleToFile filePath key epType = do
  case M.lookup key mergeData of
    Nothing -> putStrLn $ "ERROR: key " ++ show key ++ " not in mergeData map"
    Just mergeInput ->
      let (treeUserInput, flatUserInput, flatModificationInput) =
            getAllMergeInputRepresentations mergeInput
       in case epType of
            TreeUserType -> encodeFile filePath treeUserInput
            FlatUserType -> encodeFile filePath flatUserInput
            FlatModificationType -> encodeFile filePath flatModificationInput

writeSomeExamples :: IO ()
writeSomeExamples = do
  -- sound
  writeExampleToFile "./data/sound_treeuser.json" "SoundExample" TreeUserType
  writeExampleToFile "./data/sound_flatuser.json" "SoundExample" FlatUserType
  writeExampleToFile "./data/sound_flatmodification.json" "SoundExample" FlatModificationType

  -- error : missing parent feature
  writeExampleToFile "./data/errormissingparent_treeuser.json" "MissingParentFeature" TreeUserType
  writeExampleToFile "./data/errormissingparent_flatuser.json" "MissingParentFeature" FlatUserType
  writeExampleToFile "./data/errormissingparent_flatmodification.json" "MissingParentFeature" FlatModificationType
