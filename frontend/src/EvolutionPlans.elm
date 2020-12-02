module EvolutionPlans exposing (..)

import Array exposing (Array)


type alias Time =
    Int


type alias FeatureId =
    String


type alias GroupId =
    String


type alias MergeResult =
    { evolutionPlans : Array EvolutionPlan
    }


type alias EvolutionPlan =
    { timePoints : Array TimePoint
    , name : String
    }


type alias TimePoint =
    { time : Time
    , featureModel : FeatureModel
    }


type alias FeatureModel =
    { rootFeature : Feature
    }


type Feature
    = Feature FeatureFields


type alias FeatureFields =
    { id : FeatureId
    , featureType : String
    , name : String
    , groups : List Group
    }


type Group
    = Group GroupFields


type alias GroupFields =
    { id : GroupId
    , groupType : String
    , features : List Feature
    }


type Tree
    = Node
        { value : String
        , metaData : Maybe String
        , children : List Tree
        }


type ComputedTree
    = ComputedNode
        { value : String
        , metaData : Maybe String
        , children : List ComputedTree
        , computedDimentions : ComputedDimentions
        }


type alias ComputedDimentions =
    { approxNodeWidth : Float
    , treeWidth : Float
    }
