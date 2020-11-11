module Example where

import qualified Data.Map as M
import qualified Data.Set as S
import Types

baseEvolutionPlan :: AbstractedLevelEvolutionPlan FeatureModel
baseEvolutionPlan =
  AbstractedLevelEvolutionPlan
    [ TimePoint 0 fm0
    , TimePoint 1 fm1
    , TimePoint 2 fm2
    , TimePoint 3 fm3
    ]
  where
    fm0 = FeatureModel (Feature "feature:vending-machine" Mandatory "RootFeature" S.empty)
    fm1 =
      FeatureModel
        ( Feature "feature:vending-machine" Mandatory "VendingMachine" $
            S.fromList
              [ Group "group:vending-machine-group" And $
                  S.fromList [beverages]
              ]
        )
    fm2 =
      FeatureModel
        ( Feature "feature:vending-machine" Mandatory "VendingMachine" $
            S.fromList
              [ Group "group:vending-machine-group" And $
                  S.fromList [beverages, size]
              ]
        )
    fm3 =
      FeatureModel
        ( Feature "feature:vending-machine" Mandatory "VendingMachine" $
            S.fromList
              [ Group "group:vending-machine-group" And $
                  S.fromList [beverages, size, milk]
              ]
        )
    milk =
      Feature "feature:milk" Optional "Milk" $
        S.fromList
          [ Group "group:milk-group" And $
              S.fromList
                [ Feature "feature:milk-type" Optional "Milk Type" $
                    S.fromList
                      [ Group "group:milk-type-group" Or $
                          S.fromList
                            [ Feature "feature:normal" Optional "Normal" S.empty
                            , Feature "feature:lactose-free" Optional "Lactose Free" S.empty
                            , Feature "feature:soy-milk" Optional "Soy Milk" S.empty
                            ]
                      ]
                ]
          ]
    beverages =
      Feature "feature:beverages" Mandatory "Beverages" $
        S.fromList
          [ Group "group:beverages-group" And $
              S.fromList
                [ Feature "feature:tea" Mandatory "Tea" S.empty
                , Feature "feature:coffee" Mandatory "Coffee" S.empty
                ]
          ]
    size =
      Feature "feature:size" Optional "Size" $
        S.fromList
          [ Group "group:size-group" And $
              S.fromList
                [ Feature "feature:regular" Mandatory "Regular" S.empty
                , Feature "feature:other" Mandatory "Other" $
                    S.fromList
                      [ Group "group:other-group" And $
                          S.fromList
                            [ Feature "feature:small" Mandatory "Small" S.empty
                            , Feature "feature:large" Mandatory "Large" S.empty
                            ]
                      ]
                ]
          ]

v1EvolutionPlan :: AbstractedLevelEvolutionPlan FeatureModel
v1EvolutionPlan =
  AbstractedLevelEvolutionPlan
    [ TimePoint 0 fm0
    , TimePoint 1 fm1
    , TimePoint 2 fm2
    , TimePoint 3 fm3
    ]
  where
    fm0 = FeatureModel (Feature "feature:vending-machine" Mandatory "RootFeature" S.empty)
    fm1 =
      FeatureModel
        ( Feature "feature:vending-machine" Mandatory "VendingMachine" $
            S.fromList
              [ Group "group:vending-machine-group" And $
                  S.fromList [currency, beverages]
              ]
        )
    fm2 =
      FeatureModel
        ( Feature "feature:vending-machine" Mandatory "VendingMachine" $
            S.fromList
              [ Group "group:vending-machine-group" And $
                  S.fromList [currency, beverages, modifiedSize]
              ]
        )
    fm3 =
      FeatureModel
        ( Feature "feature:vending-machine" Mandatory "VendingMachine" $
            S.fromList
              [ Group "group:vending-machine-group" And $
                  S.fromList [currency, beverages, modifiedSize, modifiedMilk]
              ]
        )
    currency =
      Feature "feature:currency" Mandatory "Currency" $
        S.fromList
          [ Group "group:currency-group" Or $
              S.fromList
                [ Feature "feature:euro" Optional "Euro" S.empty
                , Feature "feature:dollar" Optional "Dollar" S.empty
                ]
          ]
    modifiedMilk =
      Feature "feature:milk" Optional "Milk" $
        S.fromList
          [ Group "group:milk-group" And $
              S.fromList
                [ Feature "feature:milk-type" Optional "Milk Type" $
                    S.fromList
                      [ Group "group:milk-type-group" And $
                          S.fromList
                            [ Feature "feature:normal" Mandatory "Normal" S.empty
                            , Feature "feature:lactose-free" Mandatory "Lactose Free" S.empty
                            ]
                      ]
                ]
          ]
    beverages =
      Feature "feature:beverages" Mandatory "Beverages" $
        S.fromList
          [ Group "group:beverages-group" And $
              S.fromList
                [ Feature "feature:tea" Mandatory "Tea" S.empty
                , Feature "feature:coffee" Mandatory "Coffee" S.empty
                ]
          ]
    modifiedSize =
      Feature "feature:size" Optional "Size" $
        S.fromList
          [ Group "group:size-group" And $
              S.fromList
                [ Feature "feature:regular" Mandatory "Regular" S.empty
                , Feature "feature:large" Mandatory "Large" S.empty
                ]
          ]

v2EvolutionPlan :: AbstractedLevelEvolutionPlan FeatureModel
v2EvolutionPlan =
  AbstractedLevelEvolutionPlan
    [ TimePoint 0 fm0
    , TimePoint 1 fm1
    , TimePoint 2 fm2
    , TimePoint 3 fm3
    ]
  where
    fm0 = FeatureModel (Feature "feature:vending-machine" Mandatory "RootFeature" S.empty)
    fm1 =
      FeatureModel
        ( Feature "feature:vending-machine" Mandatory "VendingMachine" $
            S.fromList
              [ Group "group:vending-machine-group" And $
                  S.fromList [beverages]
              ]
        )
    fm2 =
      FeatureModel
        ( Feature "feature:vending-machine" Mandatory "VendingMachine" $
            S.fromList
              [ Group "group:vending-machine-group" And $
                  S.fromList [beverages, size]
              ]
        )
    fm3 =
      FeatureModel
        ( Feature "feature:vending-machine" Mandatory "VendingMachine" $
            S.fromList
              [ Group "group:vending-machine-group" And $
                  S.fromList [beverages, size, modifiedMilk]
              ]
        )
    modifiedMilk =
      Feature "feature:milk" Optional "Milk" $
        S.fromList
          [ Group "group:milk-group" And $
              S.fromList
                [ Feature "feature:milk-type" Optional "Milk Type" $
                    S.fromList
                      [ Group "group:milk-type-group" And $
                          S.fromList
                            [ Feature "feature:normal" Mandatory "Normal" S.empty
                            , Feature "feature:lactose-free" Mandatory "Lactose Free" S.empty
                            , Feature "feature:soy-milk" Mandatory "Soy Milk" S.empty
                            ]
                      ]
                ]
          ]
    beverages =
      Feature "feature:beverages" Mandatory "Beverages" $
        S.fromList
          [ Group "group:beverages-group" And $
              S.fromList
                [ Feature "feature:tea" Mandatory "Tea" S.empty
                , Feature "feature:coffee" Mandatory "Coffee" S.empty
                , Feature "feature:cappuccino" Mandatory "Cappuccino" S.empty
                ]
          ]
    size =
      Feature "feature:size" Optional "Size" $
        S.fromList
          [ Group "group:size-group" And $
              S.fromList
                [ Feature "feature:regular" Mandatory "Regular" S.empty
                , Feature "feature:other" Mandatory "Other" $
                    S.fromList
                      [ Group "group:other-group" And $
                          S.fromList
                            [ Feature "feature:small" Mandatory "Small" S.empty
                            , Feature "feature:large" Mandatory "Large" S.empty
                            ]
                      ]
                ]
          ]

expectedEvolutionPlan :: AbstractedLevelEvolutionPlan FeatureModel
expectedEvolutionPlan =
  AbstractedLevelEvolutionPlan
    [ TimePoint 0 fm0
    , TimePoint 1 fm1
    , TimePoint 2 fm2
    , TimePoint 3 fm3
    ]
  where
    fm0 = FeatureModel (Feature "feature:vending-machine" Mandatory "RootFeature" S.empty)
    fm1 =
      FeatureModel
        ( Feature "feature:vending-machine" Mandatory "VendingMachine" $
            S.fromList
              [ Group "group:vending-machine-group" And $
                  S.fromList [currency, beverages]
              ]
        )
    fm2 =
      FeatureModel
        ( Feature "feature:vending-machine" Mandatory "VendingMachine" $
            S.fromList
              [ Group "group:vending-machine-group" And $
                  S.fromList [currency, beverages, mergedSize]
              ]
        )
    fm3 =
      FeatureModel
        ( Feature "feature:vending-machine" Mandatory "VendingMachine" $
            S.fromList
              [ Group "group:vending-machine-group" And $
                  S.fromList [currency, beverages, mergedSize, mergedMilk]
              ]
        )
    currency =
      Feature "feature:currency" Mandatory "Currency" $
        S.fromList
          [ Group "group:currency-group" Or $
              S.fromList
                [ Feature "feature:euro" Optional "Euro" S.empty
                , Feature "feature:dollar" Optional "Dollar" S.empty
                ]
          ]
    mergedMilk =
      Feature "feature:milk" Optional "Milk" $
        S.fromList
          [ Group "group:milk-group" And $
              S.fromList
                [ Feature "feature:milk-type" Optional "Milk Type" $
                    S.fromList
                      [ Group "group:milk-type-group" And $
                          S.fromList
                            [ Feature "feature:normal" Mandatory "Normal" S.empty
                            , Feature "feature:lactose-free" Mandatory "Lactose Free" S.empty
                            , Feature "feature:soy-milk" Mandatory "Soy Milk" S.empty
                            ]
                      ]
                ]
          ]
    beverages =
      Feature "feature:beverages" Mandatory "Beverages" $
        S.fromList
          [ Group "group:beverages-group" And $
              S.fromList
                [ Feature "feature:tea" Mandatory "Tea" S.empty
                , Feature "feature:coffee" Mandatory "Coffee" S.empty
                , Feature "feature:cappuccino" Mandatory "Cappuccino" S.empty
                ]
          ]
    mergedSize =
      Feature "feature:size" Optional "Size" $
        S.fromList
          [ Group "group:size-group" And $
              S.fromList
                [ Feature "feature:regular" Mandatory "Regular" S.empty
                , Feature "feature:large" Mandatory "Large" S.empty
                ]
          ]
