{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module PrettyPrint where

import Helpers
import Types

import Control.Lens
import Control.Monad.Reader
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Util

class PrettyFM a where
  prettyFM :: a -> Reader FeatureModel (Doc ann)

-- printFeatureModel :: FeatureModel -> IO ()
-- printFeatureModel fm = do
--   putDocW 50 (runReader (prettyFM fm) fm)
--   putStrLn ""

-- instance PrettyFM FeatureModel where
--   prettyFM (FM r f) = do
--     featuresDoc <- prettyFM f
--     return $
--       "FEATURE MODEL" <> line
--         <> indent
--           2
--           ( vsep
--               [ "rootId: " <> pretty r
--               , "features:"
--               , indent 2 featuresDoc
--               ]
--           )

-- instance PrettyFM FeatureTable where
--   prettyFM ft = align . vsep <$> traverse prettyMap (M.toList ft)
--     where
--       prettyMap :: (FeatureId, Feature) -> Reader FeatureModel (Doc ann)
--       prettyMap (k, v) = do
--         pv <- prettyFM v
--         return $ pretty k <+> "->" <+> pv

-- instance PrettyFM Feature where
--   prettyFM (Feature featureName mParentGroupId groups featureType) = do
--     fm <- ask
--     pgroups <- prettyFM groups
--     let parentName = mParentGroupId >>= getParentFeatureOfGroupId fm >>= preview name
--     return $
--       asList $
--         prettyArgs
--           "="
--           [ ("name", pretty featureName)
--           ,
--             ( "parentGroupId"
--             , maybe "[No Parent Group]" pretty mParentGroupId
--             )
--           , ("groups", pgroups)
--           , ("featureType", pretty (show featureType))
--           ,
--             ( "parentFeatureName"
--             , maybe "[No Parent Feature]" pretty parentName
--             )
--           ]

-- instance PrettyFM Groups where
--   prettyFM = return . mconcat . fmap (\g -> line <> prettyGroup g) . M.toList

-- prettyGroup :: (GroupId, Group) -> Doc ann
-- prettyGroup (gid, Group gType featureIds) =
--   tupled
--     [ pretty gid
--     , pretty (show gType)
--     , asCompactSet (pretty <$> S.toList featureIds)
--     ]

-- prettyArgs :: Doc ann -> [(Doc ann, Doc ann)] -> [Doc ann]
-- prettyArgs sep xs = map makeLine xs
--   where
--     makeLine (key, value) =
--       key <+> sep <> nest 4 (softline <> value)

-- asList :: [Doc ann] -> Doc ann
-- asList =
--   group . align
--     . encloseSep
--       (flatAlt "[ " "[")
--       (flatAlt (line <> "]") "]")
--       ", "

-- asCompactSet :: [Doc ann] -> Doc ann
-- asCompactSet xs =
--   "{"
--     <> align
--       (concatWith (\x y -> x <> "," <> softline <> y) xs <> "}")
