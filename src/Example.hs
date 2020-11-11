module Example where

import qualified Data.Map as M
import qualified Data.Set as S
import           Types

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

-- mod DWINPUT is including FEATUREMODEL-CHECKER . protecting STRING + NAT .
--   subsorts String < FeatureID GroupID Name .
--   subsort Nat < TimePoint .
--   op model : -> FeatureModel .
--   op plan : -> Plans .
--   op log : -> Log .
--   op init : -> TimeConfiguration .
--   eq init = currentTime: 0 C: model # done # log plan: plan .
-- eq model = FM("_eb7c1223-a2b5-4908-8601-6d303571a3a8" ,
-- ["_eb7c1223-a2b5-4908-8601-6d303571a3a8" -> f("RootFeature", noParent, noG, MANDATORY)]) .

-- eq plan = at 2 do
-- renameFeature("_eb7c1223-a2b5-4908-8601-6d303571a3a8", "VendingMachine")
-- addGroup("_eb7c1223-a2b5-4908-8601-6d303571a3a8", "_09ebe7f3-5356-4644-8557-42cb7eb7a5c3", AND)
-- addFeature("_3f58ec48-d0e7-4f0c-a09b-f920a906f830", "Beverages", "_09ebe7f3-5356-4644-8557-42cb7eb7a5c3", OPTIONAL)
-- addFeature("_fcc4d1a8-1235-4879-821c-8227722dcbac", "RingTone", "_09ebe7f3-5356-4644-8557-42cb7eb7a5c3", OPTIONAL)
-- addFeature("_c4932349-1a2e-41df-ba4f-acdf0e370c21", "Currency", "_09ebe7f3-5356-4644-8557-42cb7eb7a5c3", OPTIONAL)
-- addGroup("_3f58ec48-d0e7-4f0c-a09b-f920a906f830", "_720abe37-e4ab-4102-8fd1-8af2401c5749", AND)
-- addFeature("_a9fad2b2-453e-4a48-b254-7209f472167e", "Coffee", "_720abe37-e4ab-4102-8fd1-8af2401c5749", OPTIONAL)
-- addFeature("_f61af305-5e1f-46d6-aab2-57550217e80b", "Tea", "_720abe37-e4ab-4102-8fd1-8af2401c5749", OPTIONAL)
-- addFeature("_a105e0cb-8909-4b5d-8d1f-82d248a558af", "Cappuccino", "_720abe37-e4ab-4102-8fd1-8af2401c5749", OPTIONAL)
-- changeGroupType("_720abe37-e4ab-4102-8fd1-8af2401c5749", OR)
-- addGroup("_c4932349-1a2e-41df-ba4f-acdf0e370c21", "_8175feda-51f3-46fe-8873-7ccf8fd240fb", AND)
-- addFeature("_0970def2-87a4-4ee3-b6cb-d06e7e02d306", "Euro", "_8175feda-51f3-46fe-8873-7ccf8fd240fb", OPTIONAL)
-- addFeature("_30c1e32b-587e-41c6-8f57-ffa39cd16b7b", "Dollar", "_8175feda-51f3-46fe-8873-7ccf8fd240fb", OPTIONAL)
-- changeGroupType("_8175feda-51f3-46fe-8873-7ccf8fd240fb", ALTERNATIVE)
-- changeFeatureType("_c4932349-1a2e-41df-ba4f-acdf0e370c21", MANDATORY)
-- changeFeatureType("_3f58ec48-d0e7-4f0c-a09b-f920a906f830", MANDATORY)
-- ;
-- ;;

-- at 4 do
-- addFeature("_603f479d-e4ae-4fec-86b9-8295cb13d9da", "Size", "_09ebe7f3-5356-4644-8557-42cb7eb7a5c3", OPTIONAL)
-- addGroup("_603f479d-e4ae-4fec-86b9-8295cb13d9da", "_310e5134-497d-4460-9139-58729ab18d00", AND)
-- addFeature("_5f0c1ac5-07eb-4a6c-9f40-dc0e7969b9de", "Small", "_310e5134-497d-4460-9139-58729ab18d00", OPTIONAL)
-- addFeature("_65a98f81-7073-420e-88b7-0a8c7adc281c", "Regular", "_310e5134-497d-4460-9139-58729ab18d00", OPTIONAL)
-- addFeature("_fd3f7a58-c525-4b91-bc6b-19709fb5ad1d", "Large", "_310e5134-497d-4460-9139-58729ab18d00", OPTIONAL)
-- changeFeatureType("_5f0c1ac5-07eb-4a6c-9f40-dc0e7969b9de", MANDATORY)
-- changeFeatureType("_65a98f81-7073-420e-88b7-0a8c7adc281c", MANDATORY)
-- changeFeatureType("_fd3f7a58-c525-4b91-bc6b-19709fb5ad1d", MANDATORY)
-- ;
-- ;;

-- at 6 do
-- changeFeatureType("_5f0c1ac5-07eb-4a6c-9f40-dc0e7969b9de", OPTIONAL)
-- changeFeatureType("_fd3f7a58-c525-4b91-bc6b-19709fb5ad1d", OPTIONAL)
-- addFeature("_ef942aa4-44ac-4abb-83fd-7de422fa8657", "Other", "_310e5134-497d-4460-9139-58729ab18d00", OPTIONAL)
-- changeFeatureType("_ef942aa4-44ac-4abb-83fd-7de422fa8657", MANDATORY)
-- addGroup("_ef942aa4-44ac-4abb-83fd-7de422fa8657", "_9d8c2c78-3a71-47f4-bc99-b61827ff179d", AND)
-- moveFeature("_5f0c1ac5-07eb-4a6c-9f40-dc0e7969b9de", "_9d8c2c78-3a71-47f4-bc99-b61827ff179d")
-- moveFeature("_fd3f7a58-c525-4b91-bc6b-19709fb5ad1d", "_9d8c2c78-3a71-47f4-bc99-b61827ff179d")
-- changeGroupType("_9d8c2c78-3a71-47f4-bc99-b61827ff179d", OR)
-- ;
-- ;;

-- at 8 do
-- changeGroupType("_9d8c2c78-3a71-47f4-bc99-b61827ff179d", AND)
-- moveFeature("_fd3f7a58-c525-4b91-bc6b-19709fb5ad1d", "_310e5134-497d-4460-9139-58729ab18d00")
-- removeFeature("_5f0c1ac5-07eb-4a6c-9f40-dc0e7969b9de")
-- removeGroup("_9d8c2c78-3a71-47f4-bc99-b61827ff179d")
-- removeFeature("_ef942aa4-44ac-4abb-83fd-7de422fa8657")
-- changeFeatureType("_fd3f7a58-c525-4b91-bc6b-19709fb5ad1d", MANDATORY)
-- ;
-- ;;

-- at 10 do
-- addFeature("_4e1c7ee9-bf5b-42fc-a6fa-5d7a4db0a8e3", "Milk", "_09ebe7f3-5356-4644-8557-42cb7eb7a5c3", OPTIONAL)
-- addGroup("_65a98f81-7073-420e-88b7-0a8c7adc281c", "_7ffc5f5c-6170-47a5-b637-240adbc20326", AND)
-- addFeature("_14ff8b06-aea5-4195-953e-4f7ec74b04ce", "NewFeature1", "_7ffc5f5c-6170-47a5-b637-240adbc20326", OPTIONAL)
-- ;
-- ;;

-- at 12 do
-- addGroup("_4e1c7ee9-bf5b-42fc-a6fa-5d7a4db0a8e3", "_0481814b-5950-4979-ac4e-db0b4cd4e272", AND)
-- addFeature("_56865097-5874-4aaf-a915-0f6f7d44f528", "MilkType", "_0481814b-5950-4979-ac4e-db0b4cd4e272", OPTIONAL)
-- addGroup("_56865097-5874-4aaf-a915-0f6f7d44f528", "_2e9a7a21-e39b-4c74-8cb6-d71fd156e753", AND)
-- addFeature("_cbb02bc6-e5d0-468a-b10b-fdd299c34ee4", "Normal", "_2e9a7a21-e39b-4c74-8cb6-d71fd156e753", OPTIONAL)
-- addFeature("_cdbe9bff-1698-465e-b489-2db2b7f0d5e9", "Lactose Free", "_2e9a7a21-e39b-4c74-8cb6-d71fd156e753", OPTIONAL)
-- addFeature("_4c86bc76-91a5-4023-952d-5f911ac6eae2", "Soy Milk", "_2e9a7a21-e39b-4c74-8cb6-d71fd156e753", OPTIONAL)
-- changeFeatureType("_4c86bc76-91a5-4023-952d-5f911ac6eae2", MANDATORY)
-- changeFeatureType("_cdbe9bff-1698-465e-b489-2db2b7f0d5e9", MANDATORY)
-- changeFeatureType("_cbb02bc6-e5d0-468a-b10b-fdd299c34ee4", MANDATORY)
-- ; .

-- endm

-- rew init .
-- quit
