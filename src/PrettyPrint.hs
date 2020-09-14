module PrettyPrint where

import           Types
import           Data.Text.Prettyprint.Doc
import           Control.Lens
import           Data.Text.Prettyprint.Doc.Util

prettyType :: [Doc ann] -> Doc ann
prettyType = align . sep . zipWith (<+>) (":::" : repeat "->")

prettyDecl :: Pretty a => a -> [Doc ann] -> Doc ann
prettyDecl n tys = pretty n <+> prettyType tys

doc :: Doc ann
doc = prettyDecl ("example" :: String) ["Int", "Bool", "Char", "IO ()"]

printThings :: IO ()
printThings =
  putDocW 30 doc

printFeatureModel :: FeatureModel -> IO ()
printFeatureModel = do
  const (print "hei")
  putDocW 30 . prettyFeatureModel

prettyFeatureModel :: FeatureModel -> Doc ann
prettyFeatureModel (FM r f) = "FEATURE MODEL" <> hardline <> 
  indent 2 (vsep ["rootId: " <> pretty r, "features:", indent 2 (prettyFeatureTable f)])

prettyFeatureTable :: FeatureTable -> Doc ann
prettyFeatureTable ft = align (vsep ((\(k,v) -> (pretty k <+> "->" <+> pretty (v^.name) )) <$> kvs))
  where
    kvs = itoListOf itraversed ft
