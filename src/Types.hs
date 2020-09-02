{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Types where

import           Control.Lens
import qualified Data.Set                      as S
import qualified Data.Map                      as M


---------------------
--  FEATURE MODEL  --
---------------------


type FeatureId = Int


type RootId = FeatureId


type GroupId = Int


type FeatureTable = M.Map FeatureId Feature


type Groups = M.Map GroupId Group


data FeatureModel =
  FM 
    { _rootId :: RootId
    , _features :: FeatureTable
    }
    deriving ( Show, Read )


data Feature =
  Feature 
    { _name :: String
    , _parentGroupId :: Maybe GroupId
    , _groups :: Groups
    , _featureType :: FeatureType
    }
    deriving ( Show, Read )

data Group =
  Group 
    { _groupType :: GroupType
    , _featureIds :: S.Set FeatureId
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
    { _timePoint :: Int
    , _operations :: [Operation]
    }
  deriving ( Show, Read )


data Operation 
  = AddFeature AddFeatureOp
  | RemoveFeature RemoveFeatureOp
  | MoveFeature MoveFeatureOp
  | RenameFeature RenameFeatureOp
  | ChangeFeatureType ChangeFeatureTypeOp
  | AddGroup AddGroupOp
  | RemoveGroup RemoveGroupOp
  | ChangeGroupType ChangeGroupTypeOp
  | MoveGroup MoveGroupOp
  deriving ( Show, Read )


data AddFeatureOp = 
  AddFeatureOp 
    { _featureId :: FeatureId 
    , _name :: String 
    , _parentGroupId :: GroupId 
    , _featureType :: FeatureType
    }
  deriving ( Show, Read )


data RemoveFeatureOp = 
  RemoveFeatureOp 
    { _featureId :: FeatureId
    }
  deriving ( Show, Read )


data MoveFeatureOp = 
  MoveFeatureOp 
    { _featureId :: FeatureId
    , _groupId :: GroupId
    }
  deriving ( Show, Read )


data RenameFeatureOp = 
  RenameFeatureOp 
    { _featureId :: FeatureId
    , _name :: String
    }
  deriving ( Show, Read )


data ChangeFeatureTypeOp = 
  ChangeFeatureTypeOp 
    { _featureId :: FeatureId 
    , _featureType :: FeatureType
    }
  deriving ( Show, Read )


data AddGroupOp = 
  AddGroupOp 
    { _parentFeatureId :: FeatureId 
    , _groupId :: GroupId 
    , _groupType :: GroupType
    }
  deriving ( Show, Read )


data RemoveGroupOp = 
  RemoveGroupOp 
    { _groupId :: GroupId
    }
  deriving ( Show, Read )


data ChangeGroupTypeOp = 
  ChangeGroupTypeOp 
    { _groupId :: GroupId 
    , _groupType :: GroupType
    }
  deriving ( Show, Read )


data MoveGroupOp = 
  MoveGroupOp 
    { _groupId :: GroupId 
    , _parentFeatureId :: FeatureId
    }
  deriving ( Show, Read )


--------------
--  OPTICS  --
--------------


makeFieldsNoPrefix ''FeatureModel
makeFieldsNoPrefix ''Feature
makeFieldsNoPrefix ''Group
makePrisms ''FeatureType
makePrisms ''GroupType

makeFieldsNoPrefix ''EvolutionPlan
makeFieldsNoPrefix ''Plan
makePrisms ''Operation

makeFieldsNoPrefix ''AddFeatureOp
makeFieldsNoPrefix ''RemoveFeatureOp
makeFieldsNoPrefix ''MoveFeatureOp
makeFieldsNoPrefix ''RenameFeatureOp
makeFieldsNoPrefix ''ChangeFeatureTypeOp
makeFieldsNoPrefix ''AddGroupOp
makeFieldsNoPrefix ''RemoveGroupOp
makeFieldsNoPrefix ''ChangeGroupTypeOp
makeFieldsNoPrefix ''MoveGroupOp
