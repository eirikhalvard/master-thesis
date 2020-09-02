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


---------------------
--  FEATURE MODEL  --
---------------------


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


-----------------------
--  EVOLUTION PLANS  --
-----------------------


data EvolutionPlan =
  EvolutionPlan
    { _initialFM :: FeatureModel
    , _plans :: [Plan]
    }
  deriving ( Show, Read )

data Plan =
  Plan
    { _planTimePoint :: Int
    , _planOperations :: [Operation]
    }
  deriving ( Show, Read )

data Operation 
  = AddFeatureOp AddFeature
  | RemoveFeatureOp RemoveFeature
  | MoveFeatureOp MoveFeature
  | RenameFeatureOp RenameFeature
  | ChangeFeatureTypeOp ChangeFeatureType
  | AddGroupOp AddGroup
  | RemoveGroupOp RemoveGroup
  | ChangeGroupTypeOp ChangeGroupType
  | MoveGroupOp MoveGroup
  deriving ( Show, Read )

data AddFeature = AddFeature FeatureID String GroupID FeatureType
  deriving ( Show, Read )

data RemoveFeature = RemoveFeature FeatureID
  deriving ( Show, Read )

data MoveFeature = MoveFeature FeatureID GroupID
  deriving ( Show, Read )

data RenameFeature = RenameFeature FeatureID String
  deriving ( Show, Read )

data ChangeFeatureType = ChangeFeatureType FeatureID FeatureType
  deriving ( Show, Read )

data AddGroup = AddGroup FeatureID GroupID GroupType
  deriving ( Show, Read )

data RemoveGroup = RemoveGroup GroupID
  deriving ( Show, Read )

data ChangeGroupType = ChangeGroupType GroupID GroupType
  deriving ( Show, Read )

data MoveGroup = MoveGroup GroupID FeatureID
  deriving ( Show, Read )



--------------
--  OPTICS  --
--------------


makeLenses ''FeatureModel
makeLenses ''Feature
makeLenses ''Group
makePrisms ''FeatureType
makePrisms ''GroupType


makeLenses ''EvolutionPlan
makeLenses ''Plan
-- makeLenses ''Operation


---------------------
--  OPTIC HELPERS  --
---------------------


featuresT :: IndexedTraversal' FeatureID FeatureModel Feature
featuresT = features . itraversed

groupsT :: IndexedTraversal' GroupID Feature Group
groupsT = groups . itraversed

groupFeaturesF :: Fold Group FeatureID
groupFeaturesF = groupFeatures . folded




