module EvolutionPlansDecoder exposing (..)

import EvolutionPlans exposing (..)
import Json.Decode as Decode
import Json.Encode as Encode


mergeResult : Decode.Decoder MergeResult
mergeResult =
    Decode.map MergeResult
        (Decode.field "evolutionPlans" <| Decode.list mergeEvolutionPlan)


mergeEvolutionPlan : Decode.Decoder MergeEvolutionPlan
mergeEvolutionPlan =
    Decode.map2 MergeEvolutionPlan
        (Decode.field "name" Decode.string)
        (Decode.field "evolutionPlan" evolutionPlan)


evolutionPlan : Decode.Decoder EvolutionPlan
evolutionPlan =
    Decode.map EvolutionPlan
        (Decode.field "timePoints" <| Decode.list timePoint)


timePoint : Decode.Decoder TimePoint
timePoint =
    Decode.map2 TimePoint
        (Decode.field "time" Decode.int)
        (Decode.field "featureModel" featureModel)


featureModel : Decode.Decoder FeatureModel
featureModel =
    Decode.map FeatureModel
        (Decode.field "rootFeature" feature)


feature : Decode.Decoder Feature
feature =
    Decode.map4
        (\i f n g ->
            Feature
                { id = i
                , featureType = f
                , name = n
                , groups = g
                }
        )
        (Decode.field "id" Decode.string)
        (Decode.field "featureType" Decode.string)
        (Decode.field "name" Decode.string)
        (Decode.field "groups" <| Decode.list (Decode.lazy (\_ -> group)))


group : Decode.Decoder Group
group =
    Decode.map3
        (\i g f ->
            Group
                { id = i
                , groupType = g
                , features = f
                }
        )
        (Decode.field "id" Decode.string)
        (Decode.field "groupType" Decode.string)
        (Decode.field "features" <| Decode.list (Decode.lazy (\_ -> feature)))


tree : Decode.Decoder Tree
tree =
    Decode.map3
        (\value metaData children ->
            Node
                { value = String.fromInt value
                , metaData = metaData
                , children = children
                }
        )
        (Decode.field "value" Decode.int)
        (Decode.maybe <| Decode.field "metaData" Decode.string)
        (Decode.field "children"
            (Decode.list (Decode.lazy <| \_ -> tree))
        )
