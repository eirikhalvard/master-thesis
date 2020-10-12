module Main exposing (..)

import Debug
import Element exposing (Element, alignRight, centerY, el, fill, padding, rgb255, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Json.Decode as Decode
import Json.Encode as Encode
import Svg
import Svg.Attributes as SvgA


main =
    case Decode.decodeString decodeTree treeString of
        Err _ ->
            Element.layout [] myRowOfStuff

        Ok n ->
            Element.layout [] <|
                Element.column []
                    [ Element.text (Debug.toString n)
                    , Element.html
                        (Svg.svg []
                            [ Svg.circle
                                [ SvgA.cx "60"
                                , SvgA.cy "60"
                                , SvgA.r "50"
                                ]
                                []
                            ]
                        )
                    ]


myRowOfStuff : Element msg
myRowOfStuff =
    row [ width fill, centerY, spacing 30 ]
        [ myElement
        , myElement
        , el [ alignRight ] myElement
        ]


myElement : Element msg
myElement =
    el
        [ Background.color (rgb255 240 0 245)
        , Font.color (rgb255 255 255 255)
        , Border.rounded 3
        , padding 30
        ]
        (text "stylish!")


type Tree
    = Leaf Int
    | Node (List Tree)


treeString : String
treeString =
    """
{ "node": [
  { "node": [{"leaf": 1}]},
  { "node": [{"leaf": 2}]},
  { "node": [{"leaf": 3}, {"leaf": 4}]},
  { "leaf": 5}
] }
"""


decodeTree : Decode.Decoder Tree
decodeTree =
    Decode.oneOf
        [ Decode.map Leaf (Decode.field "leaf" Decode.int)
        , Decode.map Node
            (Decode.field "node"
                (Decode.list (Decode.lazy <| \_ -> decodeTree))
            )
        ]


encodeTree : Tree -> Encode.Value
encodeTree tree =
    case tree of
        Leaf v ->
            Encode.object [ ( "leaf", Encode.int v ) ]

        Node subtrees ->
            Encode.object [ ( "node", Encode.list encodeTree subtrees ) ]
