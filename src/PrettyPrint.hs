{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module PrettyPrint where

import           Types
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Util
import qualified Data.Map                      as M
import qualified Data.Set                      as S

printFeatureModel :: FeatureModel -> IO ()
printFeatureModel fm = do
  putDocW 50 (pretty fm)
  putStrLn ""

instance Pretty FeatureModel where
  pretty (FM r f) =
    "FEATURE MODEL" <> line <> indent
      2
      (vsep
        [ "rootId: " <> pretty r
        , "features:"
        , indent 2 (pretty f)
        ]
      )

instance Pretty FeatureTable where
  pretty ft = align
    (vsep
      (   (\(k, v) -> (pretty k <+> "->" <+> pretty v))
      <$> M.toList ft
      )
    )

instance Pretty Feature where
  pretty (Feature name parent groups featureType) =
    asList $ prettyArgs
      "="
      [ ("name", pretty name)
      , ( "parentGroupId"
        , maybe "[No Parent Group]" pretty parent
        )
      , ("groups"     , pretty groups)
      , ("featureType", pretty (show featureType))
      ]

instance Pretty Groups where
  pretty = mconcat . fmap (\g -> line <> prettyGroup g) . M.toList

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
