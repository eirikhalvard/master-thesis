{-# LANGUAGE OverloadedLists #-}

module Examples.SoundExample where

import Types

------------------------------------------------------------------------
--                     Tree Based Evolution Plan                      --
------------------------------------------------------------------------

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

------------------------------------------------------------------------
--                     Constructed Evolution Plan                     --
------------------------------------------------------------------------

baseConstructedEvolutionPlan :: TransformationEvolutionPlan Modifications FeatureModel'
baseConstructedEvolutionPlan =
  TransformationEvolutionPlan
    0
    ( FeatureModel'
        "feature:vending-machine"
        [
          ( "feature:vending-machine"
          , Feature' Nothing Mandatory "RootFeature"
          )
        ]
        []
    )
    [ Plan
        1
        ( Modifications
            [
              ( "feature:beverages"
              , FeatureAdd "group:vending-machine-group" Mandatory "Beverages"
              )
            ,
              ( "feature:coffee"
              , FeatureAdd "group:beverages-group" Optional "Coffee"
              )
            ,
              ( "feature:tea"
              , FeatureAdd "group:beverages-group" Optional "Tea"
              )
            ,
              ( "feature:vending-machine"
              , FeatureModification
                  Nothing
                  Nothing
                  (Just (FeatureNameModification "VendingMachine"))
              )
            ]
            [
              ( "group:beverages-group"
              , GroupAdd "feature:beverages" Or
              )
            ,
              ( "group:vending-machine-group"
              , GroupAdd "feature:vending-machine" And
              )
            ]
        )
    , Plan
        2
        ( Modifications
            [
              ( "feature:large"
              , FeatureAdd "group:other-group" Mandatory "Large"
              )
            ,
              ( "feature:other"
              , FeatureAdd "group:size-group" Mandatory "Other"
              )
            ,
              ( "feature:regular"
              , FeatureAdd "group:size-group" Mandatory "Regular"
              )
            ,
              ( "feature:size"
              , FeatureAdd "group:vending-machine-group" Optional "Size"
              )
            ,
              ( "feature:small"
              , FeatureAdd "group:other-group" Mandatory "Small"
              )
            ]
            [
              ( "group:other-group"
              , GroupAdd "feature:other" And
              )
            ,
              ( "group:size-group"
              , GroupAdd "feature:size" And
              )
            ]
        )
    , Plan
        3
        ( Modifications
            [
              ( "feature:lactose-free"
              , FeatureAdd "group:milk-type-group" Optional "Lactose Free"
              )
            ,
              ( "feature:milk"
              , FeatureAdd "group:vending-machine-group" Optional "Milk"
              )
            ,
              ( "feature:milk-type"
              , FeatureAdd "group:milk-group" Optional "Milk Type"
              )
            ,
              ( "feature:normal"
              , FeatureAdd "group:milk-type-group" Optional "Normal"
              )
            ,
              ( "feature:soy-milk"
              , FeatureAdd "group:milk-type-group" Optional "Soy Milk"
              )
            ]
            [
              ( "group:milk-group"
              , GroupAdd "feature:milk" And
              )
            ,
              ( "group:milk-type-group"
              , GroupAdd "feature:milk-type" Or
              )
            ]
        )
    ]

v1ConstructedEvolutionPlan :: TransformationEvolutionPlan Modifications FeatureModel'
v1ConstructedEvolutionPlan =
  TransformationEvolutionPlan
    0
    ( FeatureModel'
        "feature:vending-machine"
        [
          ( "feature:vending-machine"
          , Feature' Nothing Mandatory "RootFeature"
          )
        ]
        []
    )
    [ Plan
        1
        ( Modifications
            [
              ( "feature:beverages"
              , FeatureAdd "group:vending-machine-group" Mandatory "Beverages"
              )
            ,
              ( "feature:coffee"
              , FeatureAdd "group:beverages-group" Optional "Coffee"
              )
            ,
              ( "feature:currency"
              , FeatureAdd "group:vending-machine-group" Mandatory "Currency"
              )
            ,
              ( "feature:dollar"
              , FeatureAdd "group:currency-group" Optional "Dollar"
              )
            ,
              ( "feature:euro"
              , FeatureAdd "group:currency-group" Optional "Euro"
              )
            ,
              ( "feature:tea"
              , FeatureAdd "group:beverages-group" Optional "Tea"
              )
            ,
              ( "feature:vending-machine"
              , FeatureModification
                  Nothing
                  Nothing
                  (Just (FeatureNameModification "VendingMachine"))
              )
            ]
            [
              ( "group:beverages-group"
              , GroupAdd "feature:beverages" Or
              )
            ,
              ( "group:currency-group"
              , GroupAdd "feature:currency" Alternative
              )
            ,
              ( "group:vending-machine-group"
              , GroupAdd "feature:vending-machine" And
              )
            ]
        )
    , Plan
        2
        ( Modifications
            [
              ( "feature:large"
              , FeatureAdd "group:size-group" Mandatory "Large"
              )
            ,
              ( "feature:regular"
              , FeatureAdd "group:size-group" Mandatory "Regular"
              )
            ,
              ( "feature:size"
              , FeatureAdd "group:vending-machine-group" Optional "Size"
              )
            ]
            [
              ( "group:size-group"
              , GroupAdd "feature:size" And
              )
            ]
        )
    , Plan
        3
        ( Modifications
            [
              ( "feature:lactose-free"
              , FeatureAdd "group:milk-type-group" Mandatory "Lactose Free"
              )
            ,
              ( "feature:milk"
              , FeatureAdd "group:vending-machine-group" Optional "Milk"
              )
            ,
              ( "feature:milk-type"
              , FeatureAdd "group:milk-group" Optional "Milk Type"
              )
            ,
              ( "feature:normal"
              , FeatureAdd "group:milk-type-group" Mandatory "Normal"
              )
            ]
            [
              ( "group:milk-group"
              , GroupAdd "feature:milk" And
              )
            ,
              ( "group:milk-type-group"
              , GroupAdd "feature:milk-type" And
              )
            ]
        )
    ]

v2ConstructedEvolutionPlan :: TransformationEvolutionPlan Modifications FeatureModel'
v2ConstructedEvolutionPlan =
  TransformationEvolutionPlan
    0
    ( FeatureModel'
        "feature:vending-machine"
        [
          ( "feature:vending-machine"
          , Feature' Nothing Mandatory "RootFeature"
          )
        ]
        []
    )
    [ Plan
        1
        ( Modifications
            [
              ( "feature:beverages"
              , FeatureAdd "group:vending-machine-group" Mandatory "Beverages"
              )
            ,
              ( "feature:cappuccino"
              , FeatureAdd "group:beverages-group" Optional "Cappuccino"
              )
            ,
              ( "feature:coffee"
              , FeatureAdd "group:beverages-group" Optional "Coffee"
              )
            ,
              ( "feature:tea"
              , FeatureAdd "group:beverages-group" Optional "Tea"
              )
            ,
              ( "feature:vending-machine"
              , FeatureModification
                  Nothing
                  Nothing
                  (Just (FeatureNameModification "VendingMachine"))
              )
            ]
            [
              ( "group:beverages-group"
              , GroupAdd "feature:beverages" Or
              )
            ,
              ( "group:vending-machine-group"
              , GroupAdd "feature:vending-machine" And
              )
            ]
        )
    , Plan
        2
        ( Modifications
            [
              ( "feature:large"
              , FeatureAdd "group:other-group" Mandatory "Large"
              )
            ,
              ( "feature:other"
              , FeatureAdd "group:size-group" Mandatory "Other"
              )
            ,
              ( "feature:regular"
              , FeatureAdd "group:size-group" Mandatory "Regular"
              )
            ,
              ( "feature:size"
              , FeatureAdd "group:vending-machine-group" Optional "Size"
              )
            ,
              ( "feature:small"
              , FeatureAdd "group:other-group" Mandatory "Small"
              )
            ]
            [
              ( "group:other-group"
              , GroupAdd "feature:other" And
              )
            ,
              ( "group:size-group"
              , GroupAdd "feature:size" And
              )
            ]
        )
    , Plan
        3
        ( Modifications
            [
              ( "feature:lactose-free"
              , FeatureAdd "group:milk-type-group" Mandatory "Lactose Free"
              )
            ,
              ( "feature:milk"
              , FeatureAdd "group:vending-machine-group" Optional "Milk"
              )
            ,
              ( "feature:milk-type"
              , FeatureAdd "group:milk-group" Optional "Milk Type"
              )
            ,
              ( "feature:normal"
              , FeatureAdd "group:milk-type-group" Mandatory "Normal"
              )
            ,
              ( "feature:soy-milk"
              , FeatureAdd "group:milk-type-group" Optional "Soy Milk"
              )
            ]
            [
              ( "group:milk-group"
              , GroupAdd "feature:milk" And
              )
            ,
              ( "group:milk-type-group"
              , GroupAdd "feature:milk-type" And
              )
            ]
        )
    ]

expectedConstructedEvolutionPlan :: TransformationEvolutionPlan Modifications FeatureModel'
expectedConstructedEvolutionPlan =
  TransformationEvolutionPlan
    0
    ( FeatureModel'
        "feature:vending-machine"
        [
          ( "feature:vending-machine"
          , Feature' Nothing Mandatory "RootFeature"
          )
        ]
        []
    )
    [ Plan
        1
        ( Modifications
            [
              ( "feature:beverages"
              , FeatureAdd "group:vending-machine-group" Mandatory "Beverages"
              )
            ,
              ( "feature:cappuccino"
              , FeatureAdd "group:beverages-group" Optional "Cappuccino"
              )
            ,
              ( "feature:coffee"
              , FeatureAdd "group:beverages-group" Optional "Coffee"
              )
            ,
              ( "feature:currency"
              , FeatureAdd "group:vending-machine-group" Mandatory "Currency"
              )
            ,
              ( "feature:dollar"
              , FeatureAdd "group:currency-group" Optional "Dollar"
              )
            ,
              ( "feature:euro"
              , FeatureAdd "group:currency-group" Optional "Euro"
              )
            ,
              ( "feature:tea"
              , FeatureAdd "group:beverages-group" Optional "Tea"
              )
            ,
              ( "feature:vending-machine"
              , FeatureModification
                  Nothing
                  Nothing
                  (Just (FeatureNameModification "VendingMachine"))
              )
            ]
            [
              ( "group:beverages-group"
              , GroupAdd "feature:beverages" Or
              )
            ,
              ( "group:currency-group"
              , GroupAdd "feature:currency" Alternative
              )
            ,
              ( "group:vending-machine-group"
              , GroupAdd "feature:vending-machine" And
              )
            ]
        )
    , Plan
        2
        ( Modifications
            [
              ( "feature:large"
              , FeatureAdd "group:size-group" Mandatory "Large"
              )
            ,
              ( "feature:regular"
              , FeatureAdd "group:size-group" Mandatory "Regular"
              )
            ,
              ( "feature:size"
              , FeatureAdd "group:vending-machine-group" Optional "Size"
              )
            ]
            [
              ( "group:size-group"
              , GroupAdd "feature:size" And
              )
            ]
        )
    , Plan
        3
        ( Modifications
            [
              ( "feature:lactose-free"
              , FeatureAdd "group:milk-type-group" Mandatory "Lactose Free"
              )
            ,
              ( "feature:milk"
              , FeatureAdd "group:vending-machine-group" Optional "Milk"
              )
            ,
              ( "feature:milk-type"
              , FeatureAdd "group:milk-group" Optional "Milk Type"
              )
            ,
              ( "feature:normal"
              , FeatureAdd "group:milk-type-group" Mandatory "Normal"
              )
            ]
            [
              ( "group:milk-group"
              , GroupAdd "feature:milk" And
              )
            ,
              ( "group:milk-type-group"
              , GroupAdd "feature:milk-type" And
              )
            ]
        )
    ]
