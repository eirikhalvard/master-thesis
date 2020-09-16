module Example1
  ( carInitialModel
  , carEvolutionPlan
  , runCarExample
  )
where

import           Types
import           PrettyPrint
import           Helpers

import           Control.Lens
import qualified Data.Map                      as M
import qualified Data.Set                      as S

carInitialModel :: FeatureModel
carInitialModel = FM
  0
  (M.fromList
    [ ( 0
      , Feature "Car"
                Nothing
                (M.singleton 0 (Group And (S.singleton 1)))
                Mandatory
      )
    , ( 1
      , Feature "Infotainment System"
                (Just 0)
                (M.singleton 1 (Group And (S.singleton 2)))
                Mandatory
      )
    , (2, Feature "Bluetooth" (Just 1) M.empty Optional)
    ]
  )

carEvolutionPlan :: EvolutionPlan
carEvolutionPlan = EvolutionPlan
  0
  carInitialModel
  [ Plan
    1
    [ AddGroup (AddGroupOp 3 1 Alternative)
    , AddFeature (AddFeatureOp 3 "Android Auto" 3 Optional)
    , AddFeature (AddFeatureOp 4 "Apple Car Play" 3 Optional)
    ]
  , Plan
    2
    [ AddFeature (AddFeatureOp 5 "Comfort Systems" 0 Optional)
    , AddGroup (AddGroupOp 4 5 And)
    , AddFeature (AddFeatureOp 6 "Parking Pilot" 4 Optional)
    ]
  ]

runCarExample :: IO ()
runCarExample = do
  let printDivider s = putStr "\n\n\n--- " >> putStr s >> putStrLn "\n"

  printDivider "Car Evolution Plan"
  printFeatureModel (view initialFM carEvolutionPlan)
