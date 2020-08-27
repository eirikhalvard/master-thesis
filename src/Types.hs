{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
module Types
  ( FeatureID
  , RootID
  , GroupID
  , FeatureModel(..)
  , rootID
  , features
  , featuresT
  , FeatureTable
  , Feature(..)
  , name
  , parentID
  , groups
  , groupsT
  , featureType
  , Group(..)
  , Groups
  , groupType
  , groupFeatures
  , groupFeaturesF
  , FeatureType(..)
  , _Optional
  , _Mandatory
  , GroupType(..)
  , _And
  , _Or
  , _Alternative
  )
where

import           Control.Lens
import qualified Data.Set                      as S
import qualified Data.Map                      as M

type FeatureID = Int

type RootID = FeatureID

type GroupID = Int

type FeatureTable = M.Map FeatureID Feature

type Groups = M.Map GroupID Group

data FeatureModel =
  FM { _rootID :: RootID
     , _features :: FeatureTable
     }
     deriving ( Show, Read )

data Feature =
  Feature { _name :: String
          , _parentID :: Maybe FeatureID
          , _groups :: Groups
          , _featureType :: FeatureType
          }
          deriving ( Show, Read )

data Group =
  Group { _groupType :: GroupType
        , _groupFeatures :: S.Set FeatureID
        }
        deriving ( Show, Read )

data FeatureType
  = Optional
  | Mandatory
  deriving ( Show, Read, Eq )

data GroupType
  = And
  | Or
  | Alternative
  deriving ( Show, Read, Eq )

makeLenses ''FeatureModel
makeLenses ''Feature
makeLenses ''Group
makePrisms ''FeatureType
makePrisms ''GroupType

featuresT :: IndexedTraversal' FeatureID FeatureModel Feature
featuresT = features . itraversed

groupsT :: IndexedTraversal' GroupID Feature Group
groupsT = groups . itraversed

groupFeaturesF :: Fold Group FeatureID
groupFeaturesF = groupFeatures . folded

