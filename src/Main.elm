module Main exposing (..)

import Debug
import Element exposing (Element, alignRight, centerY, el, fill, padding, rgb255, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Json.Decode as Decode
import Json.Encode as Encode
import List
import List.Extra as List
import Svg exposing (Svg)
import Svg.Attributes as SvgA


width : Int
width =
    700


height : Int
height =
    400


nodeWidth : Int
nodeWidth =
    20


nodeHeight : Int
nodeHeight =
    20


spacingX : Int
spacingX =
    50


spacingY : Int
spacingY =
    100


main =
    case Decode.decodeString decodeTree treeString of
        Err _ ->
            Element.layout [] <| Element.text "no good"

        Ok n ->
            Element.layout [] <|
                Element.column []
                    [ Element.text (Debug.toString (computeTree n))
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
                            (n |> computeTree |> drawTree 100 100)
                         -- [ Svg.circle
                         --     [ SvgA.cx "300"
                         --     , SvgA.cy "60"
                         --     , SvgA.r "15"
                         --     ]
                         --     []
                         -- , Svg.circle
                         --     [ SvgA.cx "100"
                         --     , SvgA.cy "150"
                         --     , SvgA.r "15"
                         --     ]
                         --     []
                         -- , Svg.line
                         --     [ SvgA.x1 "300"
                         --     , SvgA.y1 "60"
                         --     , SvgA.x2 "100"
                         --     , SvgA.y2 "150"
                         --     , SvgA.strokeWidth "3"
                         --     , SvgA.stroke "blue"
                         --     ]
                         --     []
                         -- ]
                        )
                    ]


computeTree : Tree -> ComputedTree
computeTree (Node tree) =
    let
        computedChildren =
            List.map computeTree tree.children

        childrenTotalWidth =
            computedChildren
                |> List.map (\(ComputedNode t) -> t.xWidth)
                |> List.intersperse spacingX
                |> List.sum
    in
    ComputedNode
        { value = tree.value
        , metaData = tree.metaData
        , children = computedChildren
        , xWidth =
            Basics.max nodeWidth childrenTotalWidth
        }


drawTree : Int -> Int -> ComputedTree -> List (Svg msg)
drawTree xStart yStart (ComputedNode tree) =
    let
        childYLevel : Int
        childYLevel =
            yStart + spacingY

        nodeXLevel : Int
        nodeXLevel =
            xStart + (tree.xWidth // 2)

        drawChildren : Int -> List ComputedTree -> List (Svg msg)
        drawChildren childX children =
            case children of
                [] ->
                    []

                (ComputedNode child) :: rest ->
                    [ drawLine (childX + (child.xWidth // 2))
                        childYLevel
                        nodeXLevel
                        yStart
                    ]
                        ++ drawTree childX childYLevel (ComputedNode child)
                        ++ drawChildren (childX + child.xWidth + spacingX) rest
    in
    drawNode nodeXLevel yStart
        :: drawChildren xStart tree.children


drawNode : Int -> Int -> Svg msg
drawNode x y =
    Svg.circle
        [ SvgA.cx <| String.fromInt x
        , SvgA.cy <| String.fromInt y
        , SvgA.r <| String.fromInt nodeWidth
        ]
        []


drawLine : Int -> Int -> Int -> Int -> Svg msg
drawLine x1 y1 x2 y2 =
    Svg.line
        [ SvgA.x1 <| String.fromInt x1
        , SvgA.y1 <| String.fromInt y1
        , SvgA.x2 <| String.fromInt x2
        , SvgA.y2 <| String.fromInt y2
        , SvgA.strokeWidth "3"
        , SvgA.stroke "blue"
        ]
        []


type Tree
    = Node
        { value : Int
        , metaData : Maybe String
        , children : List Tree
        }


type ComputedTree
    = ComputedNode
        { value : Int
        , metaData : Maybe String
        , children : List ComputedTree
        , xWidth : Int
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
