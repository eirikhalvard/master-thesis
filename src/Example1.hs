module Example1
  ( carExample
  , runCarExample
  )
where

import           Types

import           Control.Lens
import qualified Data.Map                      as M
import qualified Data.Set                      as S

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

runCarExample :: IO ()
runCarExample = do
  let printDivider s = putStr "\n\n\n--- " >> putStr s >> putStrLn "\n"

  printDivider "Car Feature Model"
  print carExample
