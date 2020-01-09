module Types where

import           Control.Lens
import qualified Data.List                     as L
import qualified Data.Set                      as S
import qualified Data.Map                      as M
import qualified Data.Text                     as T

type FeatureID = Int
type RootID = FeatureID

type GroupID = Int

data FeatureModel =
  FM { _rootID :: RootID
     , _features :: M.Map FeatureID Feature
     }
     deriving ( Show, Read )

data Feature =
  Feature { _name :: String
          , _parentID :: FeatureID
          , _groups :: M.Map GroupID Group
          , _featureType :: FeatureType
          }
          deriving ( Show, Read )

data Group =
  Group { _groupType :: GroupType
        , _groupFeatures :: S.Set FeatureID
        }
        deriving ( Show, Read )

data FeatureType = Optional 
                 | Mandatory
                 deriving ( Show, Read, Eq )

data GroupType = And 
               | Or 
               | Alternative
               deriving ( Show, Read, Eq)

main :: IO ()
main = do
  print "hello"
