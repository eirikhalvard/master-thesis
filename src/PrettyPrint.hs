module PrettyPrint where

import           Types
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Util
import qualified Data.Map                      as M
import qualified Data.Set                      as S

prettyType :: [Doc ann] -> Doc ann
prettyType =
  align . sep . zipWith (<+>) (":::" : repeat "->")

prettyDecl :: Pretty a => a -> [Doc ann] -> Doc ann
prettyDecl n tys = pretty n <+> prettyType tys

doc :: Doc ann
doc = prettyDecl ("example" :: String)
                 ["Int", "Bool", "Char", "IO ()"]

printThings :: IO ()
printThings = putDocW 50 doc

printFeatureModel :: FeatureModel -> IO ()
printFeatureModel fm = do
  putDocW 50 (prettyFeatureModel fm)
  putStrLn ""

prettyFeatureModel :: FeatureModel -> Doc ann
prettyFeatureModel (FM r f) =
  "FEATURE MODEL" <> line <> indent
    2
    (vsep
      [ "rootId: " <> pretty r
      , "features:"
      , indent 2 (prettyFeatureTable f)
      ]
    )

prettyFeatureTable :: FeatureTable -> Doc ann
prettyFeatureTable ft = align
  (vsep
    (   (\(k, v) -> (pretty k <+> "->" <+> prettyFeature v))
    <$> M.toList ft
    )
  )

prettyFeature :: Feature -> Doc ann
prettyFeature (Feature name parent groups featureType) =
  asList $ prettyArgs
    "="
    [ ("name", pretty name)
    , ( "parentGroupId"
      , maybe "[No Parent Group]" pretty parent
      )
    , ("groups"     , prettyGroups groups)
    , ("featureType", pretty (show featureType))
    ]

prettyGroups :: Groups -> Doc ann
prettyGroups = mconcat . fmap (\g -> line <> prettyGroup g) . M.toList

prettyGroup :: (GroupId, Group) -> Doc ann
prettyGroup (gid, Group gType featureIds) =
  tupled 
    [ pretty gid
    , pretty (show gType)
    , asCompactSet (pretty <$> S.toList featureIds)
    ]

prettyArgs :: Doc ann -> [(Doc ann, Doc ann)] -> [Doc ann]
prettyArgs sep xs = map makeLine xs
  where
    makeLine (key, value) =
      key <+> sep <> nest 4 (softline <> value)


asList :: [Doc ann] -> Doc ann
asList = group . align . encloseSep
  (flatAlt "[ " "[")
  (flatAlt (line <> "]") "]")
  ", "

asCompactSet :: [Doc ann] -> Doc ann
asCompactSet xs = "{" <> align
  (concatWith (\x y -> x <> "," <> softline <> y) xs <> "}")
