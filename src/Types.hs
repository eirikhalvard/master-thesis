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
    deriving ( Show, Eq, Read )


data Feature =
  Feature
    { _name :: String
    , _parentGroupId :: Maybe GroupId
    , _groups :: Groups
    , _featureType :: FeatureType
    }
    deriving ( Show, Eq, Read )


data Group =
  Group
    { _groupType :: GroupType
    , _featureIds :: S.Set FeatureId
    }
    deriving ( Show, Eq, Read )


data FeatureType
  = Optional
  | Mandatory
  deriving ( Show, Eq, Read )


data GroupType
  = And
  | Or
  | Alternative
  deriving ( Show, Eq, Read )


-----------------------
--  EVOLUTION PLANS  --
-----------------------


data EvolutionPlan =
  EvolutionPlan
    { _initialTime :: Int
    , _initialFM :: FeatureModel
    , _plans :: [Plan]
    }
  deriving ( Show, Eq, Read )


data Plan =
  Plan
    { _timePoint :: Int
    , _operations :: [Operation]
    }
  deriving ( Show, Eq, Read )


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
  deriving ( Show, Eq, Read )


data AddFeatureOp =
  AddFeatureOp
    { _featureId :: FeatureId
    , _name :: String
    , _parentGroupId :: GroupId
    , _featureType :: FeatureType
    }
  deriving ( Show, Eq, Read )


data RemoveFeatureOp =
  RemoveFeatureOp
    { _featureId :: FeatureId
    }
  deriving ( Show, Eq, Read )


data MoveFeatureOp =
  MoveFeatureOp
    { _featureId :: FeatureId
    , _groupId :: GroupId
    }
  deriving ( Show, Eq, Read )


data RenameFeatureOp =
  RenameFeatureOp
    { _featureId :: FeatureId
    , _name :: String
    }
  deriving ( Show, Eq, Read )


data ChangeFeatureTypeOp =
  ChangeFeatureTypeOp
    { _featureId :: FeatureId
    , _featureType :: FeatureType
    }
  deriving ( Show, Eq, Read )


data AddGroupOp =
  AddGroupOp
    { _groupId :: GroupId
    , _parentFeatureId :: FeatureId
    , _groupType :: GroupType
    }
  deriving ( Show, Eq, Read )


data RemoveGroupOp =
  RemoveGroupOp
    { _groupId :: GroupId
    }
  deriving ( Show, Eq, Read )


data ChangeGroupTypeOp =
  ChangeGroupTypeOp
    { _groupId :: GroupId
    , _groupType :: GroupType
    }
  deriving ( Show, Eq, Read )


data MoveGroupOp =
  MoveGroupOp
    { _groupId :: GroupId
    , _parentFeatureId :: FeatureId
    }
  deriving ( Show, Eq, Read )


------------------
--  Validities  --
------------------


type Validity = (TimePoint, TimePoint)

type NameValidities = M.Map String [Validity]

type FeatureValidities = M.Map FeatureId FeatureValidity

type GroupValidities = M.Map GroupId GroupValidity


data TimePoint
  = TP Int
  | Forever
  deriving ( Show, Eq, Ord )


data Validities =
  Validities
    { _nameValidities :: NameValidities
    , _featureValidities :: FeatureValidities
    , _groupValidities :: GroupValidities
    }
  deriving ( Show, Eq )


data FeatureValidity =
  FeatureValidity
    { _validities :: [Validity]
    , _parentGroupValidities :: [(GroupId, Validity)]
    , _featureTypeValidities :: [(FeatureType, Validity)]
    , _childGroupValidities :: [(GroupId, Validity)]
    , _nameValidities :: [(String, Validity)]
    }
  deriving ( Show, Eq )


data GroupValidity =
  GroupValidity
    { _validities :: [Validity]
    , _parentFeatureValidities :: [(FeatureId, Validity)]
    , _groupTypeValidities :: [(GroupType, Validity)]
    , _childFeatureValidities :: [(FeatureId, Validity)]
    }
  deriving ( Show, Eq )


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

makePrisms ''TimePoint
makeFieldsNoPrefix ''Validities
makeFieldsNoPrefix ''FeatureValidity
makeFieldsNoPrefix ''GroupValidity

