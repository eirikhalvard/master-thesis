module EvolutionPlansDecoder exposing (..)

import Array
import EvolutionPlans exposing (..)
import Json.Decode as Decode
import Json.Encode as Encode


decodeResult : Decode.Decoder err -> Decode.Decoder ok -> Decode.Decoder (Result err ok)
decodeResult err ok =
    Decode.oneOf
        [ Decode.map Err <| Decode.field "Left" err
        , Decode.map Ok <| Decode.field "Right" ok
        ]


dataExamples : Decode.Decoder (DataExamples () ())
dataExamples =
    Decode.map
        DataExamples
        (Decode.field "examples" <| Decode.map Array.fromList <| Decode.list mergeResult)


mergeResult : Decode.Decoder (MergeResult () ())
mergeResult =
    Decode.map2 MergeResult
        (Decode.field "evolutionPlans" <| Decode.map Array.fromList <| Decode.list evolutionPlan)
        (Decode.field "name" Decode.string)


evolutionPlan : Decode.Decoder (EvolutionPlan () ())
evolutionPlan =
    Decode.map2 EvolutionPlan
        (Decode.field "mergeData"
            (decodeResult
                Decode.string
                (Decode.field "timePoints" <| Decode.map Array.fromList <| Decode.list timePoint)
            )
        )
        (Decode.field "name" Decode.string)


timePoint : Decode.Decoder (TimePoint () ())
timePoint =
    Decode.map2 TimePoint
        (Decode.field "time" Decode.int)
        (Decode.field "featureModel" featureModel)


featureModel : Decode.Decoder (FeatureModel () ())
featureModel =
    Decode.map FeatureModel
        (Decode.field "rootFeature" feature)


feature : Decode.Decoder (Feature () ())
feature =
    let
        field =
            Decode.map4
                (FeatureFields ())
                (Decode.field "id" Decode.string)
                (Decode.field "featureType" Decode.string)
                (Decode.field "name" Decode.string)
                (Decode.field "groups" <| Decode.list (Decode.lazy (\_ -> group)))
    in
    Decode.map Feature field


group : Decode.Decoder (Group () ())
group =
    let
        field =
            Decode.map3
                (GroupFields ())
                (Decode.field "id" Decode.string)
                (Decode.field "groupType" Decode.string)
                (Decode.field "features" <| Decode.list (Decode.lazy (\_ -> feature)))
    in
    Decode.map Group field


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
