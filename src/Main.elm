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


width : Int
width =
    700


height : Int
height =
    400


spacing : Int
spacing =
    10


main =
    case Decode.decodeString decodeTree treeString of
        Err _ ->
            Element.layout [] <| Element.text "no good"

        Ok n ->
            Element.layout [] <|
                Element.column []
                    [ Element.text (Debug.toString n)
                    , Element.html
                        (Svg.svg
                            [ SvgA.width (String.fromInt width)
                            , SvgA.height (String.fromInt height)
                            , SvgA.viewBox <|
                                "0 0 "
                                    ++ String.fromInt width
                                    ++ " "
                                    ++ String.fromInt height
                            ]
                            [ Svg.circle
                                [ SvgA.cx "300"
                                , SvgA.cy "60"
                                , SvgA.r "15"
                                ]
                                []
                            , Svg.circle
                                [ SvgA.cx "100"
                                , SvgA.cy "150"
                                , SvgA.r "15"
                                ]
                                []
                            , Svg.line
                                [ SvgA.x1 "300"
                                , SvgA.y1 "60"
                                , SvgA.x2 "100"
                                , SvgA.y2 "150"
                                , SvgA.strokeWidth "3"
                                , SvgA.stroke "blue"
                                ]
                                []
                            ]
                        )
                    ]


type Tree
    = Node
        { value : Int
        , metaData : Maybe String
        , children : List Tree
        }


decodeTree : Decode.Decoder Tree
decodeTree =
    Decode.map3
        (\value metaData children ->
            Node
                { value = value
                , metaData = metaData
                , children = children
                }
        )
        (Decode.field "value" Decode.int)
        (Decode.maybe <| Decode.field "metaData" Decode.string)
        (Decode.field "children"
            (Decode.list (Decode.lazy <| \_ -> decodeTree))
        )


encodeTree : Tree -> Encode.Value
encodeTree (Node tree) =
    let
        valuePair =
            [ ( "value", Encode.int tree.value ) ]

        metaDataPair =
            case tree.metaData of
                Nothing ->
                    []

                Just metaData ->
                    [ ( "metaData", Encode.string metaData ) ]

        childrenPair =
            [ ( "children", Encode.list encodeTree tree.children ) ]

        objectList =
            valuePair ++ metaDataPair ++ childrenPair
    in
    Encode.object objectList


treeString : String
treeString =
    """
{
  "value": 1,
  "children": [
    {
      "value": 2,
      "children": [
        {
          "value": 3,
          "children": []
        }
      ]
    },
    {
      "value": 4,
      "metaData": "Number four:)",
      "children": [
        {
          "value": 5,
          "children": []
        }
      ]
    },
    {
      "value": 6,
      "children": [
        {
          "value": 7,
          "children": []
        },
        {
          "value": 8,
          "children": []
        }
      ]
    },
    {
      "value": 9,
      "children": []
    }
  ]
}
"""
