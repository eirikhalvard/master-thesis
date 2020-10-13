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


calcWidth : ComputedTree -> Int
calcWidth (ComputedNode tree) =
    tree.xWidth
        -- a bit extra padding
        + 100


calcHeight : ComputedTree -> Int
calcHeight tree =
    depth tree * spacingY


depth : ComputedTree -> Int
depth (ComputedNode node) =
    case node.children |> List.map depth |> List.maximum of
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


type Msg
    = NoOp
    | NewWindowSize Int Int
    | GotViewport Dom.Viewport
    | GotTree (Result Http.Error Tree)


type alias Flags =
    ()


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
            convertIfInitialized <|
                UnInitialized { someFields | mDimentions = Just ( w, h ) }

        Initialized fields ->
            Initialized
                { fields
                    | width = w
                    , height = h
                    , device =
                        Element.classifyDevice
                            { width = w, height = h }
                }


convertIfInitialized : Model -> Model
convertIfInitialized model =
    case model of
        UnInitialized someFields ->
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
                    model

        _ ->
            model


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
            convertIfInitialized <|
                UnInitialized
                    { someFields | mDimentions = Just ( w, h ) }

        Initialized fields ->
            model


updateTree : Tree -> Model -> Model
updateTree tree model =
    case model of
        UnInitialized someFields ->
            convertIfInitialized <|
                UnInitialized
                    { someFields | mTree = Just tree }

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
