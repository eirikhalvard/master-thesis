{-# LANGUAGE RankNTypes #-}
module Helpers where

import           Types

import           Control.Lens
import           Control.Monad
import qualified Data.Set                      as S
import qualified Data.Map                      as M

notExists :: FeatureModel -> FeatureId -> Bool
notExists fm fid = hasn't (features . ix fid) fm

getFeature :: FeatureModel -> FeatureId -> Maybe Feature
getFeature fm fid = preview (features . ix fid) fm

getGroup :: FeatureModel -> GroupId -> Maybe Group
getGroup fm gid = preview (features . traversed . groups . ix gid) fm



getParentGroupOfFeature :: FeatureModel -> FeatureId -> Maybe GroupId
getParentGroupOfFeature fm fid =
  preview (features . ix fid . parentGroupId . _Just) fm

getParentOfGroup :: FeatureModel -> GroupId -> Maybe FeatureId
getParentOfGroup fm gid =
  preview (features . itraversed . filtered (has (groups . ix gid)) . asIndex) fm

getParentOfFeature :: FeatureModel -> FeatureId -> Maybe FeatureId
getParentOfFeature fm = 
  getParentGroupOfFeature fm >=> getParentOfGroup fm


getParentFeature :: FeatureModel -> FeatureId -> Maybe Feature
getParentFeature fm = 
  getFeature fm >=> view parentGroupId >=> getParentOfGroup fm >=> getFeature fm

getParentFeatureOfGroupId :: FeatureModel -> GroupId -> Maybe Feature
getParentFeatureOfGroupId fm = getParentOfGroup fm >=> getFeature fm

--- Optic helpers

groupOfGroupId :: GroupId -> Traversal' FeatureModel Group
groupOfGroupId gid = features . traversed . groups . ix gid

parentGroupOfFeature :: FeatureId -> Traversal' FeatureModel Group
parentGroupOfFeature fid handler fm = 
  case getParentGroupOfFeature fm fid of
    Nothing -> pure fm
    Just parentGroupId -> traverseOf (features . traversed . groups . ix parentGroupId) handler fm

parentOfGroup :: GroupId -> Traversal' FeatureModel Feature
parentOfGroup gid handler fm = 
  case getParentOfGroup fm gid of
    Nothing -> pure fm
    Just parentFid -> traverseOf (features . ix parentFid) handler fm

parentOfFeature :: FeatureId -> Traversal' FeatureModel Feature
parentOfFeature fid handler fm = 
  case getParentOfFeature fm fid of
    Nothing -> pure fm
    Just parentFid -> traverseOf (features . ix parentFid) handler fm


