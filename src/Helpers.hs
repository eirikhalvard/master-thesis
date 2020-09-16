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

getParentOfGroup :: FeatureModel -> GroupId -> Maybe FeatureId
getParentOfGroup fm gid =
  preview (features . itraversed . filtered (has (groups . ix gid)) . asIndex) fm

getParentFeature :: FeatureModel -> FeatureId -> Maybe Feature
getParentFeature fm = 
  getFeature fm >=> view parentGroupId >=> getParentOfGroup fm >=> getFeature fm

getParentFeatureOfGroupId :: FeatureModel -> GroupId -> Maybe Feature
getParentFeatureOfGroupId fm = getParentOfGroup fm >=> getFeature fm

