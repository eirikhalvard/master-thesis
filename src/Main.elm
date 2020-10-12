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


calcWidth : ComputedTree -> Int
calcWidth (ComputedNode tree) =
    tree.xWidth
        -- a bit extra padding
        + 100


calcHeight : ComputedTree -> Int
calcHeight tree =
    depth tree


depth : ComputedTree -> Int
depth (ComputedNode node) =
    case node.children |> List.map calcHeight |> List.maximum of
        Nothing ->
            1

        Just n ->
            n + 1



-- extra paddyo


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
            let
                computedTree =
                    computeTree n

                width =
                    calcWidth computedTree

                height =
                    calcHeight computedTree * spacingY
            in
            Element.layout [] <|
                Element.column [ Element.height fill ]
                    [ Element.el [ Element.width Element.fill ] <|
                        Element.text "Welcome to the tree visualizer!"
                    , Element.el
                        [ Element.clip
                        , Element.scrollbars
                        , Element.width (Element.px 800)
                        , Element.height (Element.px 600)
                        ]
                      <|
                        Element.el
                            [ Element.width <| Element.px width
                            , Element.height <| Element.px height
                            ]
                        <|
                            Element.html
                                (Svg.svg
                                    [ SvgA.width "100%"
                                    , SvgA.height "100%"
                                    , SvgA.viewBox <|
                                        "0 0 "
                                            ++ String.fromInt width
                                            ++ " "
                                            ++ String.fromInt height
                                    ]
                                    (computedTree |> drawTree nodeWidth nodeHeight)
                                )
                    , Element.el [] <| Element.text "Welcome to the bottom of the tree visualizer!"
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
        , xWidth = Basics.max nodeWidth childrenTotalWidth
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
        , SvgA.stroke "black"
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
          "children": [
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
                  "children": [
                    {
                      "value": 10,
                      "children": [
                        {
                          "value": 11,
                          "children": [
                            {
                              "value": 12,
                              "children": []
                            }
                          ]
                        },
                        {
                          "value": 13,
                          "metaData": "Number thirteen:)",
                          "children": [
                            {
                              "value": 14,
                              "children": []
                            }
                          ]
                        },
                        {
                          "value": 15,
                          "children": [
                            {
                              "value": 16,
                              "children": []
                            },
                            {
                              "value": 17,
                              "children": []
                            }
                          ]
                        },
                        {
                          "value": 18,
                          "children": [
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
                                  "children": [
                                    {
                                      "value": 10,
                                      "children": [
                                        {
                                          "value": 11,
                                          "children": [
                                            {
                                              "value": 12,
                                              "children": []
                                            }
                                          ]
                                        },
                                        {
                                          "value": 13,
                                          "metaData": "Number thirteen:)",
                                          "children": [
                                            {
                                              "value": 14,
                                              "children": []
                                            }
                                          ]
                                        },
                                        {
                                          "value": 15,
                                          "children": [
                                            {
                                              "value": 16,
                                              "children": []
                                            },
                                            {
                                              "value": 17,
                                              "children": []
                                            }
                                          ]
                                        },
                                        {
                                          "value": 18,
                                          "children": []
                                        }
                                      ]
                                    }
                                  ]
                                }
                              ]
                            }
                          ]
                        }
                      ]
                    }
                  ]
                }
              ]
            }
          ]
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
      "children": [
        {
          "value": 10,
          "children": [
            {
              "value": 11,
              "children": [
                {
                  "value": 12,
                  "children": []
                }
              ]
            },
            {
              "value": 13,
              "metaData": "Number thirteen:)",
              "children": [
                {
                  "value": 14,
                  "children": []
                }
              ]
            },
            {
              "value": 15,
              "children": [
                {
                  "value": 16,
                  "children": []
                },
                {
                  "value": 17,
                  "children": []
                }
              ]
            },
            {
              "value": 18,
              "children": [
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
                      "children": [
                        {
                          "value": 10,
                          "children": [
                            {
                              "value": 11,
                              "children": [
                                {
                                  "value": 12,
                                  "children": []
                                }
                              ]
                            },
                            {
                              "value": 13,
                              "metaData": "Number thirteen:)",
                              "children": [
                                {
                                  "value": 14,
                                  "children": []
                                }
                              ]
                            },
                            {
                              "value": 15,
                              "children": [
                                {
                                  "value": 16,
                                  "children": []
                                },
                                {
                                  "value": 17,
                                  "children": []
                                }
                              ]
                            },
                            {
                              "value": 18,
                              "children": []
                            }
                          ]
                        }
                      ]
                    }
                  ]
                }
              ]
            }
          ]
        }
      ]
    }
  ]
}
"""
