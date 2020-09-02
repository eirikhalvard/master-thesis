{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
module Validities where

import qualified Data.Map as M
import           Control.Lens
import           Types

data Timepoint = TP Int | Forever deriving (Show, Eq, Ord)

type Validity = (Timepoint, Timepoint)

type NameValidities = M.Map String [Validity]
type FeatureValidities = M.Map FeatureID FeatureValidity
type GroupValidities = M.Map GroupID GroupValidity

data AllValidities =
  AllValidities
    { _nameValidityMap :: NameValidities
    , _featureValidityMap :: FeatureValidities
    , _groupValiditiyMap :: GroupValidities
    }


data FeatureValidity = 
  FeatureValidity 
    { _featureValidities :: [Validity]
    , _parentGroupValidities :: [(GroupID, Validity)] 
    , _fTypeValidities :: [(FeatureType, Validity)]
    , _childGroupValidities :: [(GroupID, Validity)]
    }

data GroupValidity = 
  GroupValidity 
    { _groupValidities :: [Validity]
    , _parentFeatureValidities :: [(FeatureID, Validity)]
    , _gTypeValidities :: [(GroupType, Validity)]
    , _childFeatureValidities :: [(FeatureID, Validity)]
    }

makeLenses ''AllValidities
makeLenses ''FeatureValidity
makeLenses ''GroupValidity
