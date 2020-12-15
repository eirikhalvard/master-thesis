module EvolutionPlans exposing (..)

import Array exposing (Array)


type alias Time =
    Int


type alias FeatureId =
    String


type alias GroupId =
    String


type alias MergeResult extraFeatureFields extraGroupFields =
    { evolutionPlans : Array (EvolutionPlan extraFeatureFields extraGroupFields)
    }


type alias EvolutionPlan extraFeatureFields extraGroupFields =
    { timePoints : Array (TimePoint extraFeatureFields extraGroupFields)
    , name : String
    }


type alias TimePoint extraFeatureFields extraGroupFields =
    { time : Time
    , featureModel : FeatureModel extraFeatureFields extraGroupFields
    }


type alias FeatureModel extraFeatureFields extraGroupFields =
    { rootFeature : Feature extraFeatureFields extraGroupFields
    }


type Feature extraFeatureFields extraGroupFields
    = Feature (FeatureFields extraFeatureFields extraGroupFields)


type alias FeatureFields extraFeatureFields extraGroupFields =
    { extra : extraFeatureFields
    , id : FeatureId
    , featureType : String
    , name : String
    , groups : List (Group extraFeatureFields extraGroupFields)
    }


type Group extraFeatureFields extraGroupFields
    = Group (GroupFields extraFeatureFields extraGroupFields)


type alias GroupFields extraFeatureFields extraGroupFields =
    { extra : extraGroupFields
    , id : GroupId
    , groupType : String
    , features : List (Feature extraFeatureFields extraGroupFields)
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
