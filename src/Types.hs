module Types where

import           Control.Lens
import qualified Data.Set                      as S
import qualified Data.Map                      as M

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
          , _parentID :: Maybe FeatureID
          , _groups :: M.Map GroupID Group
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

carExample :: FeatureModel
carExample = FM
  0
  (M.fromList
    [ ( 1
      , Feature "Car"
                Nothing
                (M.fromList [(11, Group And (S.fromList [2]))])
                Mandatory
      )
    , ( 2
      , Feature "Infotainment System"
                (Just 1)
                (M.fromList [(21, Group And (S.fromList [3]))])
                Mandatory
      )
    , (3, Feature "Bluetooth" (Just 2) (M.fromList []) Optional)
    ]
  )

main :: IO ()
main = do
  print carExample
