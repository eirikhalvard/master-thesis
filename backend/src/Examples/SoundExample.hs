{-# LANGUAGE OverloadedLists #-}

module Examples.SoundExample where

import Types

------------------------------------------------------------------------
--                     Tree Based Evolution Plan                      --
------------------------------------------------------------------------

soundExample :: MergeInputWithExpected TreeUserEvolutionPlan
soundExample =
  MergeInputWithExpected
    (MergeInput "Sound Example" baseEvolutionPlan v1EvolutionPlan v2EvolutionPlan)
    (Right expectedEvolutionPlan)

baseEvolutionPlan :: TreeUserEvolutionPlan
baseEvolutionPlan =
  UserEvolutionPlan
    [ TimePoint 0 fm0
    , TimePoint 1 fm1
    , TimePoint 2 fm2
    , TimePoint 3 fm3
    ]
  where
    fm0 = TreeFeatureModel (TreeFeature "feature:vending-machine" Mandatory "RootFeature" [])
    fm1 =
      TreeFeatureModel
        ( TreeFeature
            "feature:vending-machine"
            Mandatory
            "VendingMachine"
            [TreeGroup "group:vending-machine-group" And [beverages]]
        )
    fm2 =
      TreeFeatureModel
        ( TreeFeature
            "feature:vending-machine"
            Mandatory
            "VendingMachine"
            [TreeGroup "group:vending-machine-group" And [beverages, size]]
        )
    fm3 =
      TreeFeatureModel
        ( TreeFeature
            "feature:vending-machine"
            Mandatory
            "VendingMachine"
            [TreeGroup "group:vending-machine-group" And [beverages, size, milk]]
        )
    milk =
      TreeFeature
        "feature:milk"
        Optional
        "Milk"
        [ TreeGroup
            "group:milk-group"
            And
            [ TreeFeature
                "feature:milk-type"
                Optional
                "Milk Type"
                [ TreeGroup
                    "group:milk-type-group"
                    Or
                    [ TreeFeature "feature:normal" Optional "Normal" []
                    , TreeFeature "feature:lactose-free" Optional "Lactose Free" []
                    , TreeFeature "feature:soy-milk" Optional "Soy Milk" []
                    ]
                ]
            ]
        ]
    beverages =
      TreeFeature
        "feature:beverages"
        Mandatory
        "Beverages"
        [ TreeGroup
            "group:beverages-group"
            Or
            [ TreeFeature "feature:tea" Optional "Tea" []
            , TreeFeature "feature:coffee" Optional "Coffee" []
            ]
        ]
    size =
      TreeFeature
        "feature:size"
        Optional
        "Size"
        [ TreeGroup
            "group:size-group"
            And
            [ TreeFeature "feature:regular" Mandatory "Regular" []
            , TreeFeature
                "feature:other"
                Mandatory
                "Other"
                [ TreeGroup
                    "group:other-group"
                    And
                    [ TreeFeature "feature:small" Mandatory "Small" []
                    , TreeFeature "feature:large" Mandatory "Large" []
                    ]
                ]
            ]
        ]

v1EvolutionPlan :: TreeUserEvolutionPlan
v1EvolutionPlan =
  UserEvolutionPlan
    [ TimePoint 0 fm0
    , TimePoint 1 fm1
    , TimePoint 2 fm2
    , TimePoint 3 fm3
    ]
  where
    fm0 = TreeFeatureModel (TreeFeature "feature:vending-machine" Mandatory "RootFeature" [])
    fm1 =
      TreeFeatureModel
        ( TreeFeature
            "feature:vending-machine"
            Mandatory
            "VendingMachine"
            [TreeGroup "group:vending-machine-group" And [currency, beverages]]
        )
    fm2 =
      TreeFeatureModel
        ( TreeFeature
            "feature:vending-machine"
            Mandatory
            "VendingMachine"
            [TreeGroup "group:vending-machine-group" And [currency, beverages, modifiedSize]]
        )
    fm3 =
      TreeFeatureModel
        ( TreeFeature
            "feature:vending-machine"
            Mandatory
            "VendingMachine"
            [TreeGroup "group:vending-machine-group" And [currency, beverages, modifiedSize, modifiedMilk]]
        )
    currency =
      TreeFeature
        "feature:currency"
        Mandatory
        "Currency"
        [ TreeGroup
            "group:currency-group"
            Alternative
            [ TreeFeature "feature:euro" Optional "Euro" []
            , TreeFeature "feature:dollar" Optional "Dollar" []
            ]
        ]
    modifiedMilk =
      TreeFeature
        "feature:milk"
        Optional
        "Milk"
        [ TreeGroup
            "group:milk-group"
            And
            [ TreeFeature
                "feature:milk-type"
                Optional
                "Milk Type"
                [ TreeGroup
                    "group:milk-type-group"
                    And
                    [ TreeFeature "feature:normal" Mandatory "Normal" []
                    , TreeFeature "feature:lactose-free" Mandatory "Lactose Free" []
                    ]
                ]
            ]
        ]
    beverages =
      TreeFeature
        "feature:beverages"
        Mandatory
        "Beverages"
        [ TreeGroup
            "group:beverages-group"
            Or
            [ TreeFeature "feature:tea" Optional "Tea" []
            , TreeFeature "feature:coffee" Optional "Coffee" []
            ]
        ]
    modifiedSize =
      TreeFeature
        "feature:size"
        Optional
        "Size"
        [ TreeGroup
            "group:size-group"
            And
            [ TreeFeature "feature:regular" Mandatory "Regular" []
            , TreeFeature "feature:large" Mandatory "Large" []
            ]
        ]

v2EvolutionPlan :: TreeUserEvolutionPlan
v2EvolutionPlan =
  UserEvolutionPlan
    [ TimePoint 0 fm0
    , TimePoint 1 fm1
    , TimePoint 2 fm2
    , TimePoint 3 fm3
    ]
  where
    fm0 = TreeFeatureModel (TreeFeature "feature:vending-machine" Mandatory "RootFeature" [])
    fm1 =
      TreeFeatureModel
        ( TreeFeature
            "feature:vending-machine"
            Mandatory
            "VendingMachine"
            [TreeGroup "group:vending-machine-group" And [beverages]]
        )
    fm2 =
      TreeFeatureModel
        ( TreeFeature
            "feature:vending-machine"
            Mandatory
            "VendingMachine"
            [TreeGroup "group:vending-machine-group" And [beverages, size]]
        )
    fm3 =
      TreeFeatureModel
        ( TreeFeature
            "feature:vending-machine"
            Mandatory
            "VendingMachine"
            [TreeGroup "group:vending-machine-group" And [beverages, size, modifiedMilk]]
        )
    modifiedMilk =
      TreeFeature
        "feature:milk"
        Optional
        "Milk"
        [ TreeGroup
            "group:milk-group"
            And
            [ TreeFeature
                "feature:milk-type"
                Optional
                "Milk Type"
                [ TreeGroup
                    "group:milk-type-group"
                    And
                    [ TreeFeature "feature:normal" Mandatory "Normal" []
                    , TreeFeature "feature:lactose-free" Mandatory "Lactose Free" []
                    , TreeFeature "feature:soy-milk" Optional "Soy Milk" []
                    ]
                ]
            ]
        ]
    beverages =
      TreeFeature
        "feature:beverages"
        Mandatory
        "Beverages"
        [ TreeGroup
            "group:beverages-group"
            Or
            [ TreeFeature "feature:tea" Optional "Tea" []
            , TreeFeature "feature:coffee" Optional "Coffee" []
            , TreeFeature "feature:cappuccino" Optional "Cappuccino" []
            ]
        ]
    size =
      TreeFeature
        "feature:size"
        Optional
        "Size"
        [ TreeGroup
            "group:size-group"
            And
            [ TreeFeature "feature:regular" Mandatory "Regular" []
            , TreeFeature
                "feature:other"
                Mandatory
                "Other"
                [ TreeGroup
                    "group:other-group"
                    And
                    [ TreeFeature "feature:small" Mandatory "Small" []
                    , TreeFeature "feature:large" Mandatory "Large" []
                    ]
                ]
            ]
        ]

expectedEvolutionPlan :: TreeUserEvolutionPlan
expectedEvolutionPlan =
  UserEvolutionPlan
    [ TimePoint 0 fm0
    , TimePoint 1 fm1
    , TimePoint 2 fm2
    , TimePoint 3 fm3
    ]
  where
    fm0 = TreeFeatureModel (TreeFeature "feature:vending-machine" Mandatory "RootFeature" [])
    fm1 =
      TreeFeatureModel
        ( TreeFeature
            "feature:vending-machine"
            Mandatory
            "VendingMachine"
            [TreeGroup "group:vending-machine-group" And [currency, beverages]]
        )
    fm2 =
      TreeFeatureModel
        ( TreeFeature
            "feature:vending-machine"
            Mandatory
            "VendingMachine"
            [TreeGroup "group:vending-machine-group" And [currency, beverages, mergedSize]]
        )
    fm3 =
      TreeFeatureModel
        ( TreeFeature
            "feature:vending-machine"
            Mandatory
            "VendingMachine"
            [TreeGroup "group:vending-machine-group" And [currency, beverages, mergedSize, mergedMilk]]
        )
    currency =
      TreeFeature
        "feature:currency"
        Mandatory
        "Currency"
        [ TreeGroup
            "group:currency-group"
            Alternative
            [ TreeFeature "feature:euro" Optional "Euro" []
            , TreeFeature "feature:dollar" Optional "Dollar" []
            ]
        ]
    mergedMilk =
      TreeFeature
        "feature:milk"
        Optional
        "Milk"
        [ TreeGroup
            "group:milk-group"
            And
            [ TreeFeature
                "feature:milk-type"
                Optional
                "Milk Type"
                [ TreeGroup
                    "group:milk-type-group"
                    And
                    [ TreeFeature "feature:normal" Mandatory "Normal" []
                    , TreeFeature "feature:lactose-free" Mandatory "Lactose Free" []
                    ]
                ]
            ]
        ]
    beverages =
      TreeFeature
        "feature:beverages"
        Mandatory
        "Beverages"
        [ TreeGroup
            "group:beverages-group"
            Or
            [ TreeFeature "feature:tea" Optional "Tea" []
            , TreeFeature "feature:coffee" Optional "Coffee" []
            , TreeFeature "feature:cappuccino" Optional "Cappuccino" []
            ]
        ]
    mergedSize =
      TreeFeature
        "feature:size"
        Optional
        "Size"
        [ TreeGroup
            "group:size-group"
            And
            [ TreeFeature "feature:regular" Mandatory "Regular" []
            , TreeFeature "feature:large" Mandatory "Large" []
            ]
        ]

------------------------------------------------------------------------
--                     Constructed Evolution Plan                     --
------------------------------------------------------------------------

soundConstructedExample :: MergeInputWithExpected FlatModificationEvolutionPlan
soundConstructedExample =
  MergeInputWithExpected
    ( MergeInput
        "Sound Example"
        baseConstructedEvolutionPlan
        v1ConstructedEvolutionPlan
        v2ConstructedEvolutionPlan
    )
    (Right expectedConstructedEvolutionPlan)

baseConstructedEvolutionPlan :: FlatModificationEvolutionPlan
baseConstructedEvolutionPlan =
  TransformationEvolutionPlan
    0
    ( FlatFeatureModel
        "feature:vending-machine"
        [
          ( "feature:vending-machine"
          , FlatFeature Nothing Mandatory "RootFeature"
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

v1ConstructedEvolutionPlan :: FlatModificationEvolutionPlan
v1ConstructedEvolutionPlan =
  TransformationEvolutionPlan
    0
    ( FlatFeatureModel
        "feature:vending-machine"
        [
          ( "feature:vending-machine"
          , FlatFeature Nothing Mandatory "RootFeature"
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

v2ConstructedEvolutionPlan :: FlatModificationEvolutionPlan
v2ConstructedEvolutionPlan =
  TransformationEvolutionPlan
    0
    ( FlatFeatureModel
        "feature:vending-machine"
        [
          ( "feature:vending-machine"
          , FlatFeature Nothing Mandatory "RootFeature"
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

expectedConstructedEvolutionPlan :: FlatModificationEvolutionPlan
expectedConstructedEvolutionPlan =
  TransformationEvolutionPlan
    0
    ( FlatFeatureModel
        "feature:vending-machine"
        [
          ( "feature:vending-machine"
          , FlatFeature Nothing Mandatory "RootFeature"
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
