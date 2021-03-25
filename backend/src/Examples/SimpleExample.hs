{-# LANGUAGE OverloadedLists #-}

module Examples.SimpleExample where

import Types

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

simpleExampleModV1 :: FlatModificationEvolutionPlan
simpleExampleModV1 =
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
        ,
          ( "feature4"
          , FeatureAdd "group" Optional "Feature 4"
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
              (Just (GroupTypeModification Alternative))
          )
        ]

simpleExampleModV2 :: FlatModificationEvolutionPlan
simpleExampleModV2 =
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
        [("feature3", FeatureRemove)]
        [
          ( "group"
          , GroupModification
              Nothing
              (Just (GroupTypeModification Or))
          )
        ]

simpleExampleMergedPlan :: MergeEvolutionPlan FlatFeatureModel
simpleExampleMergedPlan =
  TransformationEvolutionPlan
    0
    initial
    [Plan 1 diffResult1, Plan 2 diffResult2]
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
    diffResult1 =
      DiffResult
        [ ("feature2", NoChange (FeatureAdd "group" Optional "Feature 2"))
        , ("feature3", NoChange (FeatureAdd "group" Mandatory "Feature 3"))
        , ("feature4", ChangedInOne V1 (OneChangeWithoutBase (AddedModification (FeatureAdd "group" Optional "Feature 4"))))
        ]
        [("group", NoChange (GroupAdd "rootFeature" And))]
    diffResult2 =
      DiffResult
        [ ("feature3", NoChange FeatureRemove)
        , ("rootFeature", ChangedInOne V2 (OneChangeWithBase (FeatureModification Nothing Nothing (Just (FeatureNameModification "Root Feature"))) RemovedModification))
        ]
        [ ("group", ChangedInOne V1 (OneChangeWithBase (GroupModification Nothing (Just (GroupTypeModification Or))) (ChangedModification (GroupModification Nothing (Just (GroupTypeModification Alternative))))))
        ]

simpleExampleUnifiedPlan :: Either Conflict FlatModificationEvolutionPlan
simpleExampleUnifiedPlan =
  Right $
    TransformationEvolutionPlan
      0
      initial
      [ Plan 1 modifications1
      , Plan 2 modifications2
      ]
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
        ,
          ( "feature4"
          , FeatureAdd "group" Optional "Feature 4"
          )
        ]
        [("group", GroupAdd "rootFeature" And)]
    modifications2 =
      Modifications
        [("feature3", FeatureRemove)]
        [
          ( "group"
          , GroupModification
              Nothing
              (Just (GroupTypeModification Alternative))
          )
        ]

simpleExampleCheckedPlan :: Either Conflict FlatUserEvolutionPlan
simpleExampleCheckedPlan =
  Right $
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
          , FlatFeature
              Nothing
              Mandatory
              "Feature 1"
          )
        ]
        []
    fm1 =
      FlatFeatureModel
        "rootFeature"
        [
          ( "feature2"
          , FlatFeature
              (Just "group")
              Optional
              "Feature 2"
          )
        ,
          ( "feature3"
          , FlatFeature
              (Just "group")
              Mandatory
              "Feature 3"
          )
        ,
          ( "feature4"
          , FlatFeature
              (Just "group")
              Optional
              "Feature 4"
          )
        ,
          ( "rootFeature"
          , FlatFeature
              Nothing
              Mandatory
              "Feature 1"
          )
        ]
        [
          ( "group"
          , FlatGroup
              "rootFeature"
              And
          )
        ]
    fm2 =
      FlatFeatureModel
        "rootFeature"
        [
          ( "feature2"
          , FlatFeature
              (Just "group")
              Optional
              "Feature 2"
          )
        ,
          ( "feature4"
          , FlatFeature
              (Just "group")
              Optional
              "Feature 4"
          )
        ,
          ( "rootFeature"
          , FlatFeature
              Nothing
              Mandatory
              "Feature 1"
          )
        ]
        [
          ( "group"
          , FlatGroup
              "rootFeature"
              Alternative
          )
        ]

generatedDependencies :: [(Time, [Dependency])]
generatedDependencies =
  [
    ( 0
    ,
      [ FeatureDependency
          (FeatureAdd "group" Optional "Feature 2")
          (ParentGroupExists "group")
      , FeatureDependency
          (FeatureAdd "group" Optional "Feature 2")
          (UniqueName "Feature 2")
      , FeatureDependency
          (FeatureAdd "group" Optional "Feature 2")
          (FeatureIsWellFormed "feature2")
      , FeatureDependency
          (FeatureAdd "group" Mandatory "Feature 3")
          (ParentGroupExists "group")
      , FeatureDependency
          (FeatureAdd "group" Mandatory "Feature 3")
          (UniqueName "Feature 3")
      , FeatureDependency
          (FeatureAdd "group" Mandatory "Feature 3")
          (FeatureIsWellFormed "feature3")
      , FeatureDependency
          (FeatureAdd "group" Optional "Feature 4")
          (ParentGroupExists "group")
      , FeatureDependency
          (FeatureAdd "group" Optional "Feature 4")
          (UniqueName "Feature 4")
      , FeatureDependency
          (FeatureAdd "group" Optional "Feature 4")
          (FeatureIsWellFormed "feature4")
      , GroupDependency
          (GroupAdd "rootFeature" And)
          (ParentFeatureExists "rootFeature")
      ]
    )
  ,
    ( 1
    ,
      [ FeatureDependency
          FeatureRemove
          (NoChildGroups "feature3")
      , GroupDependency
          (GroupModification Nothing (Just (GroupTypeModification Alternative)))
          (GroupIsWellFormed "group")
      ]
    )
  ]
