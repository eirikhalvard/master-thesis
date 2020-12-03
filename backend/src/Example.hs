{-# LANGUAGE OverloadedLists #-}

module Example where

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
    fm0 = FeatureModel (Feature "feature:vending-machine" Mandatory "RootFeature" [])
    fm1 =
      FeatureModel
        ( Feature
            "feature:vending-machine"
            Mandatory
            "VendingMachine"
            [Group "group:vending-machine-group" And [beverages]]
        )
    fm2 =
      FeatureModel
        ( Feature
            "feature:vending-machine"
            Mandatory
            "VendingMachine"
            [Group "group:vending-machine-group" And [beverages, size]]
        )
    fm3 =
      FeatureModel
        ( Feature
            "feature:vending-machine"
            Mandatory
            "VendingMachine"
            [Group "group:vending-machine-group" And [beverages, size, milk]]
        )
    milk =
      Feature
        "feature:milk"
        Optional
        "Milk"
        [ Group
            "group:milk-group"
            And
            [ Feature
                "feature:milk-type"
                Optional
                "Milk Type"
                [ Group
                    "group:milk-type-group"
                    Or
                    [ Feature "feature:normal" Optional "Normal" []
                    , Feature "feature:lactose-free" Optional "Lactose Free" []
                    , Feature "feature:soy-milk" Optional "Soy Milk" []
                    ]
                ]
            ]
        ]
    beverages =
      Feature
        "feature:beverages"
        Mandatory
        "Beverages"
        [ Group
            "group:beverages-group"
            Or
            [ Feature "feature:tea" Optional "Tea" []
            , Feature "feature:coffee" Optional "Coffee" []
            ]
        ]
    size =
      Feature
        "feature:size"
        Optional
        "Size"
        [ Group
            "group:size-group"
            And
            [ Feature "feature:regular" Mandatory "Regular" []
            , Feature
                "feature:other"
                Mandatory
                "Other"
                [ Group
                    "group:other-group"
                    And
                    [ Feature "feature:small" Mandatory "Small" []
                    , Feature "feature:large" Mandatory "Large" []
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
    fm0 = FeatureModel (Feature "feature:vending-machine" Mandatory "RootFeature" [])
    fm1 =
      FeatureModel
        ( Feature
            "feature:vending-machine"
            Mandatory
            "VendingMachine"
            [Group "group:vending-machine-group" And [currency, beverages]]
        )
    fm2 =
      FeatureModel
        ( Feature
            "feature:vending-machine"
            Mandatory
            "VendingMachine"
            [Group "group:vending-machine-group" And [currency, beverages, modifiedSize]]
        )
    fm3 =
      FeatureModel
        ( Feature
            "feature:vending-machine"
            Mandatory
            "VendingMachine"
            [Group "group:vending-machine-group" And [currency, beverages, modifiedSize, modifiedMilk]]
        )
    currency =
      Feature
        "feature:currency"
        Mandatory
        "Currency"
        [ Group
            "group:currency-group"
            Alternative
            [ Feature "feature:euro" Optional "Euro" []
            , Feature "feature:dollar" Optional "Dollar" []
            ]
        ]
    modifiedMilk =
      Feature
        "feature:milk"
        Optional
        "Milk"
        [ Group
            "group:milk-group"
            And
            [ Feature
                "feature:milk-type"
                Optional
                "Milk Type"
                [ Group
                    "group:milk-type-group"
                    And
                    [ Feature "feature:normal" Mandatory "Normal" []
                    , Feature "feature:lactose-free" Mandatory "Lactose Free" []
                    ]
                ]
            ]
        ]
    beverages =
      Feature
        "feature:beverages"
        Mandatory
        "Beverages"
        [ Group
            "group:beverages-group"
            Or
            [ Feature "feature:tea" Optional "Tea" []
            , Feature "feature:coffee" Optional "Coffee" []
            ]
        ]
    modifiedSize =
      Feature
        "feature:size"
        Optional
        "Size"
        [ Group
            "group:size-group"
            And
            [ Feature "feature:regular" Mandatory "Regular" []
            , Feature "feature:large" Mandatory "Large" []
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
    fm0 = FeatureModel (Feature "feature:vending-machine" Mandatory "RootFeature" [])
    fm1 =
      FeatureModel
        ( Feature
            "feature:vending-machine"
            Mandatory
            "VendingMachine"
            [Group "group:vending-machine-group" And [beverages]]
        )
    fm2 =
      FeatureModel
        ( Feature
            "feature:vending-machine"
            Mandatory
            "VendingMachine"
            [Group "group:vending-machine-group" And [beverages, size]]
        )
    fm3 =
      FeatureModel
        ( Feature
            "feature:vending-machine"
            Mandatory
            "VendingMachine"
            [Group "group:vending-machine-group" And [beverages, size, modifiedMilk]]
        )
    modifiedMilk =
      Feature
        "feature:milk"
        Optional
        "Milk"
        [ Group
            "group:milk-group"
            And
            [ Feature
                "feature:milk-type"
                Optional
                "Milk Type"
                [ Group
                    "group:milk-type-group"
                    And
                    [ Feature "feature:normal" Mandatory "Normal" []
                    , Feature "feature:lactose-free" Mandatory "Lactose Free" []
                    , Feature "feature:soy-milk" Mandatory "Soy Milk" []
                    ]
                ]
            ]
        ]
    beverages =
      Feature
        "feature:beverages"
        Mandatory
        "Beverages"
        [ Group
            "group:beverages-group"
            Or
            [ Feature "feature:tea" Optional "Tea" []
            , Feature "feature:coffee" Optional "Coffee" []
            , Feature "feature:cappuccino" Optional "Cappuccino" []
            ]
        ]
    size =
      Feature
        "feature:size"
        Optional
        "Size"
        [ Group
            "group:size-group"
            And
            [ Feature "feature:regular" Mandatory "Regular" []
            , Feature
                "feature:other"
                Mandatory
                "Other"
                [ Group
                    "group:other-group"
                    And
                    [ Feature "feature:small" Mandatory "Small" []
                    , Feature "feature:large" Mandatory "Large" []
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
    fm0 = FeatureModel (Feature "feature:vending-machine" Mandatory "RootFeature" [])
    fm1 =
      FeatureModel
        ( Feature
            "feature:vending-machine"
            Mandatory
            "VendingMachine"
            [Group "group:vending-machine-group" And [currency, beverages]]
        )
    fm2 =
      FeatureModel
        ( Feature
            "feature:vending-machine"
            Mandatory
            "VendingMachine"
            [Group "group:vending-machine-group" And [currency, beverages, mergedSize]]
        )
    fm3 =
      FeatureModel
        ( Feature
            "feature:vending-machine"
            Mandatory
            "VendingMachine"
            [Group "group:vending-machine-group" And [currency, beverages, mergedSize, mergedMilk]]
        )
    currency =
      Feature
        "feature:currency"
        Mandatory
        "Currency"
        [ Group
            "group:currency-group"
            Alternative
            [ Feature "feature:euro" Optional "Euro" []
            , Feature "feature:dollar" Optional "Dollar" []
            ]
        ]
    mergedMilk =
      Feature
        "feature:milk"
        Optional
        "Milk"
        [ Group
            "group:milk-group"
            And
            [ Feature
                "feature:milk-type"
                Optional
                "Milk Type"
                [ Group
                    "group:milk-type-group"
                    And
                    [ Feature "feature:normal" Mandatory "Normal" []
                    , Feature "feature:lactose-free" Mandatory "Lactose Free" []
                    , Feature "feature:soy-milk" Mandatory "Soy Milk" []
                    ]
                ]
            ]
        ]
    beverages =
      Feature
        "feature:beverages"
        Mandatory
        "Beverages"
        [ Group
            "group:beverages-group"
            Or
            [ Feature "feature:tea" Optional "Tea" []
            , Feature "feature:coffee" Optional "Coffee" []
            , Feature "feature:cappuccino" Optional "Cappuccino" []
            ]
        ]
    mergedSize =
      Feature
        "feature:size"
        Optional
        "Size"
        [ Group
            "group:size-group"
            And
            [ Feature "feature:regular" Mandatory "Regular" []
            , Feature "feature:large" Mandatory "Large" []
            ]
        ]
