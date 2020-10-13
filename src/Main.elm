module Main exposing (..)

import Browser
import Browser.Dom as Dom
import Browser.Events as E
import Debug
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html exposing (Html)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import List
import List.Extra as List
import Svg exposing (Svg)
import Svg.Attributes as SvgA
import Task


calcWidth : ComputedTree -> Float
calcWidth (ComputedNode tree) =
    tree.computedDimentions.treeWidth
        -- a bit extra padding
        + 100


calcHeight : ComputedTree -> Float
calcHeight tree =
    let
        treeDepth =
            toFloat <| depth tree

        nodeTotalHeight =
            (treeDepth - 1) * nodeHeight

        spacingTotalHeight =
            (treeDepth - 1) * spacingY
    in
    treeDepth + nodeTotalHeight + spacingTotalHeight + 100


depth : ComputedTree -> Int
depth (ComputedNode node) =
    case node.children |> List.map depth |> List.maximum of
        Nothing ->
            1

        Just n ->
            n + 1



-- extra paddyo


nodeHeight : Float
nodeHeight =
    30


spacingX : Float
spacingX =
    30


spacingY : Float
spacingY =
    80


type Msg
    = NoOp
    | NewWindowSize Int Int
    | GotViewport Dom.Viewport
    | GotTree (Result Http.Error Tree)


type alias Flags =
    {}


type Model
    = UnInitialized SomeFields
    | Initialized Fields


type alias SomeFields =
    { mDimentions : Maybe ( Int, Int )
    , mTree : Maybe Tree
    }


type alias Fields =
    { device : Element.Device
    , width : Int
    , height : Int
    , tree : Tree
    }


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        getViewport =
            Task.perform GotViewport Dom.getViewport

        getTree =
            Http.get
                { url = "./tree.json"
                , expect =
                    Http.expectJson
                        GotTree
                        decodeTree
                }
    in
    ( UnInitialized
        { mTree = Nothing
        , mDimentions = Nothing
        }
    , Cmd.batch [ getViewport, getTree ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        NewWindowSize w h ->
            ( updateWindowSize w h model, Cmd.none )

        GotViewport viewport ->
            ( updateViewport (Debug.log "viewPort: " viewport) model, Cmd.none )

        GotTree treeErr ->
            case treeErr of
                Err _ ->
                    ( model, Cmd.none )

                Ok t ->
                    ( updateTree t model, Cmd.none )


updateWindowSize : Int -> Int -> Model -> Model
updateWindowSize w h model =
    case model of
        UnInitialized someFields ->
            convertIfInitialized
                { someFields | mDimentions = Just ( w, h ) }

        Initialized fields ->
            Initialized
                { fields
                    | width = w
                    , height = h
                    , device =
                        Element.classifyDevice
                            { width = w, height = h }
                }


convertIfInitialized : SomeFields -> Model
convertIfInitialized someFields =
    case ( someFields.mDimentions, someFields.mTree ) of
        ( Just ( w, h ), Just tree ) ->
            Initialized
                { width = w
                , height = h
                , device =
                    Element.classifyDevice
                        { width = w, height = h }
                , tree = tree
                }

        _ ->
            UnInitialized someFields


updateViewport : Dom.Viewport -> Model -> Model
updateViewport viewport model =
    let
        w =
            round viewport.viewport.width

        h =
            round viewport.viewport.height
    in
    case model of
        UnInitialized someFields ->
            convertIfInitialized
                { someFields | mDimentions = Just ( w, h ) }

        Initialized fields ->
            model


updateTree : Tree -> Model -> Model
updateTree tree model =
    case model of
        UnInitialized someFields ->
            convertIfInitialized { someFields | mTree = Just tree }

        Initialized fields ->
            model


subscriptions : Model -> Sub Msg
subscriptions model =
    E.onResize (\w h -> NewWindowSize w h)


view : Model -> Html Msg
view model =
    case model of
        UnInitialized someFields ->
            Element.layout [] <| Element.text <| Debug.toString someFields

        Initialized fields ->
            let
                computedTree =
                    computeTree fields.tree

                width =
                    calcWidth computedTree

                height =
                    calcHeight computedTree
            in
            Element.layout [] <|
                Element.column [ Element.height Element.fill ]
                    [ Element.el [ Element.width Element.fill ] <|
                        Element.text "Welcome to the tree visualizer!"
                    , Element.text (Debug.toString model)
                    , Element.el
                        [ Element.clip
                        , Element.scrollbars
                        , Element.width (Element.px <| fields.width - 100)
                        , Element.height (Element.px <| fields.height - 100)
                        ]
                      <|
                        Element.el
                            [ Element.width <| Element.px <| round width
                            , Element.height <| Element.px <| round height
                            ]
                        <|
                            Element.html
                                (Svg.svg
                                    [ SvgA.width "100%"
                                    , SvgA.height "100%"
                                    , SvgA.viewBox <|
                                        "0 0 "
                                            ++ String.fromFloat width
                                            ++ " "
                                            ++ String.fromFloat height
                                    ]
                                    (computedTree |> drawTree 0 0)
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
                |> List.map (\(ComputedNode t) -> t.computedDimentions.treeWidth)
                |> List.intersperse spacingX
                |> List.sum

        approxNodeWidth =
            toFloat (String.length tree.value) * 15
    in
    ComputedNode
        { value = tree.value
        , metaData = tree.metaData
        , children = computedChildren
        , computedDimentions =
            { approxNodeWidth = approxNodeWidth
            , treeWidth = Basics.max approxNodeWidth childrenTotalWidth
            }
        }


drawTree : Float -> Float -> ComputedTree -> List (Svg msg)
drawTree xStart yStart (ComputedNode tree) =
    let
        childYLevel : Float
        childYLevel =
            yStart + spacingY

        nodeXLevel : Float
        nodeXLevel =
            xStart + (tree.computedDimentions.treeWidth / 2)

        drawChildren : Float -> List ComputedTree -> List (Svg msg)
        drawChildren childX children =
            case children of
                [] ->
                    []

                (ComputedNode child) :: rest ->
                    [ drawLine
                        nodeXLevel
                        (yStart + nodeHeight)
                        (childX + (child.computedDimentions.treeWidth / 2))
                        childYLevel
                    ]
                        ++ drawTree childX childYLevel (ComputedNode child)
                        ++ drawChildren
                            (childX
                                + child.computedDimentions.treeWidth
                                + spacingX
                            )
                            rest
    in
    drawNode nodeXLevel yStart (ComputedNode tree)
        :: drawChildren xStart tree.children


drawNode : Float -> Float -> ComputedTree -> Svg msg
drawNode x y (ComputedNode node) =
    Svg.g
        [ SvgA.transform <|
            "translate("
                ++ String.fromFloat x
                ++ ", "
                ++ String.fromFloat y
                ++ ")"
        ]
        [ Svg.rect
            [ SvgA.width <|
                String.fromFloat
                    node.computedDimentions.approxNodeWidth
            , SvgA.height <| String.fromFloat nodeHeight
            , SvgA.x <|
                String.fromFloat <|
                    -(node.computedDimentions.approxNodeWidth / 2)
            , SvgA.fill "lightgrey"
            ]
            []
        , Svg.text_
            [ SvgA.x "0"
            , SvgA.y <| String.fromFloat <| nodeHeight / 2
            , SvgA.dominantBaseline "middle"
            , SvgA.textAnchor "middle"
            ]
            [ Svg.text node.value ]
        ]


drawLine : Float -> Float -> Float -> Float -> Svg msg
drawLine x1 y1 x2 y2 =
    Svg.line
        [ SvgA.x1 <| String.fromFloat x1
        , SvgA.y1 <| String.fromFloat y1
        , SvgA.x2 <| String.fromFloat x2
        , SvgA.y2 <| String.fromFloat y2
        , SvgA.strokeWidth "3"
        , SvgA.stroke "black"
        ]
        []


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


decodeTree : Decode.Decoder Tree
decodeTree =
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
            (Decode.list (Decode.lazy <| \_ -> decodeTree))
        )
