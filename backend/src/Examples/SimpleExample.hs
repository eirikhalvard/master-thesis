{-# LANGUAGE OverloadedLists #-}

module Examples.SoundExample where

import Types

import qualified Data.Set as Set

------------------------------------------------------------------------
--                     Tree Based Evolution Plan                      --
------------------------------------------------------------------------

simpleExample :: TreeUserEvolutionPlan
simpleExample =
  UserEvolutionPlan
    [ TimePoint 0 fm0
    , TimePoint 1 fm1
    , TimePoint 2 fm2
    ]
  where
    fm0 =
      TreeFeatureModel
        ( TreeFeature
            "rootFeature"
            Mandatory
            "Feature 1"
            []
        )
    fm1 =
      TreeFeatureModel
        ( TreeFeature
            "rootFeature"
            Mandatory
            "Feature 1"
            [ TreeGroup
                "group"
                And
                [ TreeFeature
                    "feature2"
                    Optional
                    "Feature 2"
                    []
                , TreeFeature
                    "feature3"
                    Mandatory
                    "Feature 3"
                    []
                ]
            ]
        )
    fm2 =
      TreeFeatureModel
        ( TreeFeature
            "rootFeature"
            Mandatory
            "Root Feature"
            [ TreeGroup
                "group"
                Or
                [ TreeFeature
                    "feature2"
                    Optional
                    "Feature 2"
                    []
                ]
            ]
        )

simpleExampleFlat :: FlatUserEvolutionPlan
simpleExampleFlat =
  UserEvolutionPlan
    [ TimePoint 0 fm0
    , TimePoint 1 fm1
    , TimePoint 2 fm2
    ]
  where
    fm0 =
      FlatFeatureModel
        "rootFeature"
        [
          ( "rootFeature"
          , FlatFeature Nothing Mandatory "Feature 1"
          )
        ]
        []

    fm1 =
      FlatFeatureModel
        "rootFeature"
        [
          ( "feature2"
          , FlatFeature (Just "group") Optional "Feature 2"
          )
        ,
          ( "feature3"
          , FlatFeature (Just "group") Mandatory "Feature 3"
          )
        ,
          ( "rootFeature"
          , FlatFeature Nothing Mandatory "Feature 1"
          )
        ]
        [("group", FlatGroup "rootFeature" And)]
    fm2 =
      FlatFeatureModel
        "rootFeature"
        [
          ( "feature2"
          , FlatFeature (Just "group") Optional "Feature 2"
          )
        ,
          ( "rootFeature"
          , FlatFeature Nothing Mandatory "Root Feature"
          )
        ]
        [("group", FlatGroup "rootFeature" Or)]

simpleExampleMod :: FlatModificationEvolutionPlan
simpleExampleMod =
  TransformationEvolutionPlan
    0
    initial
    [Plan 1 modifications1, Plan 2 modifications2]
  where
    initial =
      FlatFeatureModel
        "rootFeature"
        [
          ( "rootFeature"
          , FlatFeature Nothing Mandatory "Feature 1"
          )
        ]
        []
    modifications1 =
      Modifications
        [
          ( "feature2"
          , FeatureAdd "group" Optional "Feature 2"
          )
        ,
          ( "feature3"
          , FeatureAdd "group" Mandatory "Feature 3"
          )
        ]
        [("group", GroupAdd "rootFeature" And)]
    modifications2 =
      Modifications
        [ ("feature3", FeatureRemove)
        ,
          ( "rootFeature"
          , FeatureModification
              Nothing
              Nothing
              (Just (FeatureNameModification "Root Feature"))
          )
        ]
        [
          ( "group"
          , GroupModification
              Nothing
              (Just (GroupTypeModification Or))
          )
        ]
