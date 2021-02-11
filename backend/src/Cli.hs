module Cli where

import Types

import Data.Char
import Options.Applicative

executeParser :: IO CliOptions
executeParser = execParser parserWithInfo

parserWithInfo :: ParserInfo CliOptions
parserWithInfo =
  info
    (parser <**> helper)
    ( fullDesc
        <> progDesc "Merges evolution plans into a single merged plan, which respects the formal semantics of evolution plans"
        <> header "A three way merge tool for feature model evolution plans"
    )

parser :: Parser CliOptions
parser =
  CliOptions
    <$> parseMode
    <*> parseFromType
    <*> parseToType
    <*> parsePrint
    <*> parseGenerateElm
    <*> parseToFile

parseMode :: Parser Mode
parseMode = generateOne <|> generateAll <|> fromFile
  where
    generateOne =
      GenerateOne
        <$> strOption
          ( long "generateOne"
              <> metavar "EXAMPLENAME"
              <> help "Generates one of the examples"
          )
    generateAll =
      flag'
        GenerateAll
        ( long "generateAll"
            <> help "Generates all examples"
        )
    fromFile =
      FromFile
        <$> strOption
          ( long "fromFile"
              <> metavar "FILENAME"
              <> help "Read a three way merge plan from file"
          )

parseFromType :: Parser EvolutionPlanType
parseFromType =
  option
    (maybeReader toEvolutionPlanType)
    ( long "fromType"
        <> short 'F'
        <> metavar "FROMTYPE"
        <> help
          ( "The type to convert from (useful only when reading from file)"
              ++ "(choices: TreeUser | FlatUser | FlatModification)"
          )
        <> value TreeUserType
        <> showDefaultWith fromEvolutionPlanType
    )

parseToType :: Parser EvolutionPlanType
parseToType =
  option
    (maybeReader toEvolutionPlanType)
    ( long "toType"
        <> short 'T'
        <> metavar "TOTYPE"
        <> help
          ( "The type to convert to (useful when printing and writing to file)"
              ++ "(choices: TreeUser | FlatUser | FlatModification)"
          )
        <> value FlatModificationType
        <> showDefaultWith fromEvolutionPlanType
    )

fromEvolutionPlanType :: EvolutionPlanType -> String
fromEvolutionPlanType epType = case epType of
  TreeUserType -> "TreeUser"
  FlatUserType -> "FlatUser"
  FlatModificationType -> "FlatModification"

toEvolutionPlanType :: String -> Maybe EvolutionPlanType
toEvolutionPlanType str = case map toUpper str of
  "TREEUSER" -> Just TreeUserType
  "FLATUSER" -> Just FlatUserType
  "FLATMODIFICATION" -> Just FlatModificationType
  _ -> Nothing

parsePrint :: Parser Bool
parsePrint =
  switch
    ( long "print"
        <> short 'p'
        <> help "Whether to print the merge result"
    )

parseGenerateElm :: Parser Bool
parseGenerateElm =
  switch
    ( long "generateElm"
        <> short 'g'
        <> help "Whether to pass generated results to the elm frontend"
    )

parseToFile :: Parser (Maybe FilePath)
parseToFile =
  optional $
    strOption
      ( long "toFile"
          <> short 'o'
          <> metavar "FILEPATH"
          <> help "Outputed file to write the merge result as JSON"
      )
