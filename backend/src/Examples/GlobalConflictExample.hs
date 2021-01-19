module Examples.GlobalConflictExample where

import Examples.SoundExample

-- data Conflict
--   = Merge Time MergeConflict
--   | Local Time LocalConflict
--   | Global Time GlobalConflict
--   | Panic Time String
--   deriving (Show, Eq)

-- data GlobalConflict
--   = FailedDependencies [Dependency]
--   deriving (Show, Eq)

-- data Dependency
--   = FeatureDependency FeatureModification FeatureDependencyType
--   | GroupDependency GroupModification GroupDependencyType
--   deriving (Show, Eq)

-- data FeatureDependencyType
--   = NoChildGroups FeatureId
--   | ParentGroupExists GroupId
--   | NoCycleFromFeature FeatureId
--   | FeatureIsWellFormed FeatureId
--   | UniqueName String
--   deriving (Show, Eq)

-- data GroupDependencyType
--   = NoChildFeatures GroupId
--   | ParentFeatureExists FeatureId
--   | NoCycleFromGroup GroupId
--   | GroupIsWellFormed GroupId
--   deriving (Show, Eq)
